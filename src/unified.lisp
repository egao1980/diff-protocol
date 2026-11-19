(in-package #:diff-protocol)

;;; R1 :UNIFIED render / parse (brief: "Unified format"). Text diffs are bare
;;; @@ hunks; tree diffs are `diff --git` sections, one per file.

(defclass %parsed-opaque (opaque-change)
  ((mode :initarg :mode :initform nil :reader %parsed-opaque-mode))
  (:documentation "A `Binary files … differ` or `copy from` / `copy to` section from
PARSE-DIFF :UNIFIED; MODE is the new / deleted file mode of its header, kept so the
section renders back unchanged. A copy is an :ADD whose CHANGE-FROM is the source."))

;;; C-quoted paths (git's quote.c). Git writes a path between double quotes, with C
;;; escapes, when it holds a control character, DEL, '"' or '\' — and, with
;;; core.quotePath=true (its default), any byte >= #x80, as \ooo octets of the
;;; UTF-8 name. Only these escapes exist: \a \b \t \n \v \f \r \" \\ and
;;; \ooo (first digit 0-3). Rendering quotes as git does with core.quotePath=off:
;;; non-ASCII characters are written as they are.

(defun %utf-8-octets (char)
  "The UTF-8 octets of CHAR, a list."
  (let ((code (char-code char)))
    (cond ((< code #x80) (list code))
          ((< code #x800) (list (logior #xC0 (ash code -6))
                                (logior #x80 (logand code #x3F))))
          ((< code #x10000) (list (logior #xE0 (ash code -12))
                                  (logior #x80 (logand (ash code -6) #x3F))
                                  (logior #x80 (logand code #x3F))))
          (t (list (logior #xF0 (ash code -18))
                   (logior #x80 (logand (ash code -12) #x3F))
                   (logior #x80 (logand (ash code -6) #x3F))
                   (logior #x80 (logand code #x3F)))))))

(defun %utf-8-decode (octets)
  "OCTETS (a vector) decoded as UTF-8 → string, or NIL when they are not valid UTF-8."
  (let ((n (length octets))
        (i 0))
    (with-output-to-string (out)
      (loop while (< i n)
            do (let* ((b (aref octets i))
                      (len (cond ((< b #x80) 1)
                                 ((= (logand b #xE0) #xC0) 2)
                                 ((= (logand b #xF0) #xE0) 3)
                                 ((= (logand b #xF8) #xF0) 4)
                                 (t (return-from %utf-8-decode nil))))
                      (code (if (= len 1) b (logand b (ash #xFF (- (1+ len)))))))
                 (when (> (+ i len) n)
                   (return-from %utf-8-decode nil))
                 (loop for k from (1+ i) below (+ i len)
                       for c = (aref octets k)
                       do (unless (= (logand c #xC0) #x80)
                            (return-from %utf-8-decode nil))
                          (setf code (logior (ash code 6) (logand c #x3F))))
                 (when (or (< code (case len (1 0) (2 #x80) (3 #x800) (t #x10000)))
                           (<= #xD800 code #xDFFF)
                           (> code #x10FFFF)
                           (>= code char-code-limit))
                   (return-from %utf-8-decode nil))
                 (write-char (code-char code) out)
                 (incf i len))))))

(defun %c-unquote (string start)
  "STRING[START] is a double quote: → (values path end), END the index after the
closing quote. Escaped octets are decoded as UTF-8 (each octet as one character when
they are not valid UTF-8). NIL when the quoted string is malformed."
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (n (length string))
        (i (1+ start)))
    (flet ((octal-digit (k)
             (and (< k n) (digit-char-p (char string k) 8))))
      (loop
        (when (>= i n)
          (return-from %c-unquote nil))
        (let ((c (char string i)))
          (cond
            ((char= c #\")
             (return))
            ((char= c #\\)
             (when (>= (1+ i) n)
               (return-from %c-unquote nil))
             (let* ((e (char string (1+ i)))
                    (simple (case e
                              (#\a 7) (#\b 8) (#\t 9) (#\n 10) (#\v 11) (#\f 12) (#\r 13)
                              (#\" 34) (#\\ 92))))
               (cond (simple
                      (vector-push-extend simple octets)
                      (incf i 2))
                     ((and (digit-char-p e 4) (octal-digit (+ i 2)) (octal-digit (+ i 3)))
                      (vector-push-extend (+ (* 64 (digit-char-p e 8))
                                             (* 8 (octal-digit (+ i 2)))
                                             (octal-digit (+ i 3)))
                                          octets)
                      (incf i 4))
                     (t (return-from %c-unquote nil)))))
            (t
             (dolist (b (%utf-8-octets c))
               (vector-push-extend b octets))
             (incf i))))))
    (values (or (%utf-8-decode octets) (map 'string #'code-char octets))
            (1+ i))))

(defun %maybe-unquote (string)
  "STRING as written in a header line → the path: unquoted when it starts with a double
quote (which must then end STRING), as it is otherwise. :MALFORMED for a bad quoted path."
  (if (%prefix-p "\"" string)
      (multiple-value-bind (path end) (%c-unquote string 0)
        (if (and path (= end (length string))) path :malformed))
      string))

(defun %c-quote-p (char)
  (let ((code (char-code char)))
    (or (< code 32) (= code 127) (char= char #\") (char= char #\\))))

(defun %git-path (prefix path)
  "PREFIX and PATH as git writes them in a header: C-quoted (prefix inside the quotes)
when PATH holds a control character, DEL, '\"' or '\\'."
  (if (notany #'%c-quote-p path)
      (concatenate 'string prefix path)
      (with-output-to-string (s)
        (write-char #\" s)
        (write-string prefix s)
        (loop for c across path
              for code = (char-code c)
              do (let ((named (case code
                                (7 #\a) (8 #\b) (9 #\t) (10 #\n) (11 #\v) (12 #\f) (13 #\r)
                                (34 #\") (92 #\\))))
                   (cond (named (write-char #\\ s) (write-char named s))
                         ((%c-quote-p c) (format s "\\~3,'0O" code))
                         (t (write-char c s)))))
        (write-char #\" s))))

;;; Hunks

(defparameter *no-newline-marker* "\\ No newline at end of file")

(defun %unified-range (start count)
  "Header range from a 0-based START: 1-based; count 1 omitted; count 0 → the line before."
  (case count
    (0 (format nil "~D,0" start))
    (1 (format nil "~D" (1+ start)))
    (t (format nil "~D,~D" (1+ start) count))))

(defun %write-unified-line (prefix line s)
  (write-string prefix s)
  (write-string line s)
  (unless (%has-newline-p line)
    (terpri s)
    (write-line *no-newline-marker* s)))

(defun %write-hunk-body (algorithm before after s)
  "Common lines \" \", each changed run as \"-\" lines then \"+\" lines."
  (let* ((av (coerce before 'simple-vector))
         (bv (coerce after 'simple-vector))
         (pos 0))
    (dolist (r (%line-runs algorithm av bv))
      (destructuring-bind (i0 i1 j0 j1) r
        (loop for k from pos below i0 do (%write-unified-line " " (svref av k) s))
        (loop for k from i0 below i1 do (%write-unified-line "-" (svref av k) s))
        (loop for k from j0 below j1 do (%write-unified-line "+" (svref bv k) s))
        (setf pos i1)))
    (loop for k from pos below (length av) do (%write-unified-line " " (svref av k) s))))

(defun %hunk-start (h)
  (car (last (change-path h))))

(defun %render-hunks (e algorithm hunks s)
  "HUNKS in diff order: descending (paths are old starts), ascending (an inverse: paths
are new starts) or a single hunk. Any other order → UNSUPPORTED-CONTENT."
  (let* ((ps (mapcar #'%hunk-start hunks))
         (order (cond ((notevery #'integerp ps)
                       (error 'unsupported-content :engine e :content hunks))
                      ((null (rest ps)) :single)
                      ((apply #'> ps) :old)
                      ((apply #'< ps) :new)
                      (t (error 'unsupported-content :engine e :content hunks))))
         (delta 0))
    (dolist (h (if (eq order :old) (reverse hunks) hunks))
      (let* ((p (%hunk-start h))
             (before (change-before h))
             (after (change-after h))
             (lb (length before))
             (la (length after))
             (old (if (eq order :new) (- p delta) p))
             (new (if (eq order :old) (+ p delta) p)))
        (format s "@@ -~A +~A @@~%" (%unified-range old lb) (%unified-range new la))
        (%write-hunk-body algorithm before after s)
        (incf delta (- la lb))))))

(defun %text-algorithm (e)
  (if (typep e 'text-diff-engine) (text-engine-algorithm e) :myers))

;;; Text diffs

(defun %text-diff-hunks (e d)
  "The hunks to render for text diff D: a root change renders its children, or a
context-0 diff of its sides when it has none (:SUMMARY)."
  (let ((changes (diff-changes d)))
    (when (some (lambda (c) (typep c 'opaque-change)) changes)
      (error 'unsupported-content :engine e :content d :diff d))
    (cond ((and changes (null (rest changes)) (null (change-path (first changes))))
           (let ((root (first changes)))
             (or (change-children root)
                 (diff-changes (compute-diff e (change-before root) (change-after root))))))
          ((some (lambda (c) (null (change-path c))) changes)
           (error 'unsupported-content :engine e :content d :diff d))
          (t changes))))

(defun %render-text-unified (e d s)
  (let ((hunks (%text-diff-hunks e d)))
    (when hunks
      (%render-hunks e (%text-algorithm e) hunks s))))

(defmethod render-unified-content ((e text-diff-engine) before after changes stream)
  ;; Line hunks: CHANGES (this engine's hunks) when given, else a line diff of the sides.
  (when (and (typep before '(or null string)) (typep after '(or null string)))
    (let ((hunks (or changes (diff-changes (compute-diff e (or before "") (or after ""))))))
      (when hunks
        (%render-hunks e (text-engine-algorithm e) hunks stream))
      t)))

;;; Tree diffs: sections

(defstruct (%section (:constructor %make-section (key)))
  key kind from old-node new-node old-mode new-mode file-mode hunks binary-p
  ;; Non-text leaf changes, and the file nodes around them when the diff has them.
  leaf-changes leaf-before leaf-after)

(defun %text-node-p (node)
  (and (tree-file-p node) (eq :text (tree-file-type node)) (stringp (tree-file-content node))))

(defun %box-value (box)
  (and (%side-available-p box) (%force box)))

(defun %tree-sections (d)
  "Sections of tree diff D keyed by file path (a move by its destination), sorted."
  (let ((sections '()))
    (labels ((section (names)
               (or (find names sections :key #'%section-key :test #'equal)
                   (let ((sec (%make-section names)))
                     (push sec sections)
                     sec)))
             (leaf (c)
               (let ((sec (section (%tree-names (change-path c)))))
                 (unless (%section-kind sec) (setf (%section-kind sec) :modify))
                 (cond ((%mode-change-p c)
                        (setf (%section-old-mode sec) (change-before c)
                              (%section-new-mode sec) (change-after c)))
                       ((some (lambda (f) (and (> (length f) 5) (string= "text:" f :end2 5)))
                              (change-footprint c))
                        (setf (%section-hunks sec) (append (%section-hunks sec) (list c))))
                       (t (setf (%section-leaf-changes sec)
                                (append (%section-leaf-changes sec) (list c))
                                (%section-binary-p sec) t)))))
             (walk (c)
               (cond
                 ((typep c 'opaque-change)
                  (let ((sec (section (%tree-names (change-path c)))))
                    (setf (%section-kind sec) (change-kind c)
                          (%section-binary-p sec) t)
                    (when (and (typep c '%parsed-opaque) (eq :add (change-kind c)) (change-from c))
                      (setf (%section-kind sec) :copy
                            (%section-from sec) (change-from c)))
                    (when (typep c '%parsed-opaque)
                      (setf (%section-file-mode sec) (%parsed-opaque-mode c)))))
                 ((%leaf-change-p c) (leaf c))
                 ((and (eq :modify (change-kind c)) (change-children c))
                  (let ((sec (section (change-path c))))
                    (setf (%section-leaf-before sec) (%box-value (%before-box c))
                          (%section-leaf-after sec) (%box-value (%after-box c))))
                  (mapc #'walk (change-children c)))
                 (t
                  (let ((sec (section (change-path c))))
                    (setf (%section-kind sec) (change-kind c)
                          (%section-from sec) (change-from c)
                          (%section-old-node sec) (unless (eq :add (change-kind c))
                                                    (%box-value (%before-box c)))
                          (%section-new-node sec) (unless (eq :remove (change-kind c))
                                                    (%box-value (%after-box c)))))))))
      (mapc #'walk (diff-changes d)))
    (sort sections #'string< :key (lambda (sec) (%path-string (%section-key sec))))))

(defun %leaf-text-engine (e)
  (let ((leaf (%leaf-engine e :text)))
    (if (typep leaf 'text-diff-engine) leaf (find-diff-engine :text))))

(defun %scheme-leaf-engine (e c)
  "The leaf engine of E whose ENGINE-NAME is the scheme of leaf change C's footprint
(\"lisp:/…\" → :LISP), or NIL."
  (let* ((f (find-if-not (lambda (f) (%prefix-p "tree:" f)) (change-footprint c)))
         (colon (and f (position #\: f))))
    (when colon
      (let ((scheme (subseq f 0 colon)))
        (loop for (nil . spec) in (tree-engine-leaf-engines e)
              for leaf = (if (symbolp spec) (and spec (find-diff-engine spec)) spec)
              when (and leaf (string-equal scheme (symbol-name (engine-name leaf))))
                return leaf)))))

(defun %content-of (node)
  (and (tree-file-p node) (tree-file-content node)))

(defun %section-leaf (e sec kind on nn)
  "→ (values leaf-engine before after changes) to render a non-text section with
RENDER-UNIFIED-CONTENT, or NIL when no leaf engine applies."
  (let ((changes (%section-leaf-changes sec)))
    (cond (changes
           (let* ((lb (%section-leaf-before sec))
                  (la (%section-leaf-after sec))
                  (leaf (if (tree-file-p lb)
                            (%leaf-engine e (tree-file-type lb))
                            (%scheme-leaf-engine e (first changes))))
                  (n (length (%section-key sec))))
             (values leaf (%content-of lb) (%content-of la)
                     (mapcar (lambda (c) (%unnest c n)) changes))))
          ((case kind
             (:add (tree-file-p nn))
             (:remove (tree-file-p on))
             ((:move :modify) (and (tree-file-p on) (tree-file-p nn)
                                   (eq (tree-file-type on) (tree-file-type nn)))))
           (values (%leaf-engine e (tree-file-type (if (tree-file-p on) on nn)))
                   (%content-of on) (%content-of nn) nil)))))

(defun %render-section (e sec s)
  (let* ((kind (%section-kind sec))
         (new (%path-string (%section-key sec)))
         (old (if (%section-from sec) (%path-string (%section-from sec)) new))
         (on (%section-old-node sec))
         (nn (%section-new-node sec))
         (text (%leaf-text-engine e))
         (hunks (%section-hunks sec))
         (binary (%section-binary-p sec))
         (old-mode (%section-old-mode sec))
         (new-mode (%section-new-mode sec))
         (body nil))
    ;; Whole nodes (add / remove / move / modify without children): derive the rest.
    (flet ((content-hunks (a b)
             (diff-changes (compute-diff text a b))))
      (case kind
        (:add (when nn
                (if (%text-node-p nn)
                    (setf hunks (content-hunks "" (tree-file-content nn)))
                    (setf binary t))))
        (:remove (when on
                   (if (%text-node-p on)
                       (setf hunks (content-hunks (tree-file-content on) ""))
                       (setf binary t))))
        ((:move :modify)
         (when (and on nn)
           (unless (eql (%file-mode on) (%file-mode nn))
             (setf old-mode (%file-mode on) new-mode (%file-mode nn)))
           (cond ((and (%text-node-p on) (%text-node-p nn))
                  (setf hunks (content-hunks (tree-file-content on) (tree-file-content nn))))
                 ((and (eq (tree-file-type on) (tree-file-type nn))
                       (%content-equal e (tree-file-type on)
                                       (tree-file-content on) (tree-file-content nn))))
                 (t (setf binary t)))))))
    ;; A non-text file: its leaf engine's unified form, when it has one (#7).
    (when binary
      (multiple-value-bind (leaf before after changes) (%section-leaf e sec kind on nn)
        (when leaf
          (let* ((ok nil)
                 (out (with-output-to-string (o)
                        (setf ok (render-unified-content leaf before after changes o)))))
            (when ok
              (setf binary nil body out))))))
    (format s "diff --git ~A ~A~%" (%git-path "a/" old) (%git-path "b/" new))
    (when (and old-mode new-mode (not (member kind '(:add :remove))))
      (format s "old mode ~O~%new mode ~O~%" old-mode new-mode))
    (case kind
      (:add (format s "new file mode ~O~%"
                    (or (and nn (%file-mode nn)) (%section-file-mode sec) #o100644)))
      (:remove (format s "deleted file mode ~O~%"
                       (or (and on (%file-mode on)) (%section-file-mode sec) #o100644)))
      (:move
       (when (and on nn)
         (format s "similarity index ~D%~%"
                 (if (and (%text-node-p on) (%text-node-p nn))
                     (floor (* 100 (%similarity (tree-file-content on) (tree-file-content nn))))
                     (if (%file-equal e (%file-with on :mode nil) (%file-with nn :mode nil)) 100 0))))
       (format s "rename from ~A~%rename to ~A~%" (%git-path "" old) (%git-path "" new)))
      (:copy
       (format s "copy from ~A~%copy to ~A~%" (%git-path "" old) (%git-path "" new))))
    (let ((a (if (eq kind :add) "/dev/null" (%git-path "a/" old)))
          (b (if (eq kind :remove) "/dev/null" (%git-path "b/" new))))
      (cond ((eq kind :copy))
            (binary (format s "Binary files ~A and ~A differ~%" a b))
            (hunks (format s "--- ~A~%+++ ~A~%" a b)
                   (%render-hunks e (text-engine-algorithm text) hunks s))
            ((plusp (length body))
             (format s "--- ~A~%+++ ~A~%" a b)
             (write-string body s))))))

(defun %render-tree-unified (e d s)
  (dolist (sec (%tree-sections d))
    (%render-section e sec s)))

(defmethod render-diff ((d diff) (format (eql :unified)) stream)
  (let* ((e (diff-engine d))
         (text (with-output-to-string (s)
                 (typecase e
                   (text-diff-engine (%render-text-unified e d s))
                   (tree-diff-engine (%render-tree-unified e d s))
                   (t (unless (and (typep e 'diff-engine)
                                   (render-unified-content e nil nil (%leaves (diff-changes d)) s))
                        (error 'unsupported-content :engine e :content d :diff d)))))))
    (%call-with-output stream (lambda (s) (write-string text s)))))

;;; Parse

(defun %parse-error (line text)
  (error 'diff-parse-error :line line :text text))

(defun %chomp (line)
  (if (%has-newline-p line) (subseq line 0 (1- (length line))) line))

(defun %prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun %parse-hunk-header (l)
  "\"@@ -a[,b] +c[,d] @@…\" → (values a b c d), or NIL."
  (let ((pos 0)
        (len (length l)))
    (labels ((fail () (return-from %parse-hunk-header nil))
             (expect (s)
               (if (and (<= (+ pos (length s)) len)
                        (string= s l :start2 pos :end2 (+ pos (length s))))
                   (incf pos (length s))
                   (fail)))
             (num ()
               (let ((start pos))
                 (loop while (and (< pos len) (digit-char-p (char l pos))) do (incf pos))
                 (if (= start pos) (fail) (parse-integer l :start start :end pos))))
             (range ()
               (let ((a (num)))
                 (if (and (< pos len) (char= #\, (char l pos)))
                     (progn (incf pos) (values a (num)))
                     (values a 1)))))
      (expect "@@ -")
      (multiple-value-bind (os oc) (range)
        (expect " +")
        (multiple-value-bind (ns nc) (range)
          (expect " @@")
          (values os oc ns nc))))))

(defun %parse-hunk (lines i)
  "Hunk whose header is LINES[I] → (values change next-index). Body lines are read
while the header's counts are unfinished."
  (let ((n (length lines))
        (header (%chomp (svref lines i))))
    (multiple-value-bind (os oc ns nc) (%parse-hunk-header header)
      (declare (ignore ns))
      (unless os (%parse-error (1+ i) header))
      (let ((old '()) (new '()) (last nil) (k (1+ i))
            (p (if (zerop oc) os (1- os))))
        (flet ((strip-newline ()
                 (when (member last '(:both :old)) (setf (first old) (%chomp (first old))))
                 (when (member last '(:both :new)) (setf (first new) (%chomp (first new))))))
          (loop while (or (plusp oc) (plusp nc))
                do (when (>= k n) (%parse-error (1+ k) nil))
                   (let* ((l (svref lines k))
                          (c (and (plusp (length l)) (char l 0)))
                          (content (subseq l (min 1 (length l)))))
                     (case c
                       (#\Space (unless (and (plusp oc) (plusp nc)) (%parse-error (1+ k) l))
                        (push content old) (push content new)
                        (decf oc) (decf nc) (setf last :both))
                       (#\- (unless (plusp oc) (%parse-error (1+ k) l))
                        (push content old) (decf oc) (setf last :old))
                       (#\+ (unless (plusp nc) (%parse-error (1+ k) l))
                        (push content new) (decf nc) (setf last :new))
                       (#\\ (unless last (%parse-error (1+ k) l))
                        (strip-newline))
                       (t (%parse-error (1+ k) l))))
                   (incf k))
          (when (and (< k n) (%prefix-p "\\" (svref lines k)))
            (unless last (%parse-error (1+ k) (svref lines k)))
            (strip-newline)
            (incf k)))
        (values (%make-hunk p (reverse old) (reverse new))
                k)))))

(defstruct (%psection (:constructor %make-psection ()))
  git-old git-new old-path new-path saw-minus saw-plus
  rename-from rename-to copy-from copy-to new-file-mode deleted-file-mode old-mode new-mode binary
  hunks)

(defun %strip-path (s)
  "A ---/+++ path: C-quoted → unquoted (anything after the closing quote dropped);
otherwise text after a TAB dropped and /dev/null → NIL. Then one leading component
stripped (like patch -p1). :MALFORMED for a bad quoted path."
  (let ((s (if (%prefix-p "\"" s)
               (or (values (%c-unquote s 0))
                   (return-from %strip-path :malformed))
               (let ((s (subseq s 0 (or (position #\Tab s) (length s)))))
                 (if (string= s "/dev/null")
                     (return-from %strip-path nil)
                     s)))))
    (let ((slash (position #\/ s)))
      (if slash (subseq s (1+ slash)) s))))

(defun %split-names (string)
  (loop with start = 0
        for pos = (position #\/ string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun %octal (string)
  (parse-integer string :radix 8 :junk-allowed t))

(defun %section-changes (sec)
  (let* ((old (or (%psection-rename-from sec)
                  (if (%psection-saw-minus sec) (%psection-old-path sec) (%psection-git-old sec))))
         (new (or (%psection-rename-to sec)
                  (if (%psection-saw-plus sec) (%psection-new-path sec) (%psection-git-new sec))))
         (added (or (%psection-new-file-mode sec)
                    (and (%psection-saw-minus sec) (null (%psection-old-path sec)))))
         (deleted (or (%psection-deleted-file-mode sec)
                      (and (%psection-saw-plus sec) (null (%psection-new-path sec)))))
         (hunks (%psection-hunks sec))      ; descending (pushed in file order)
         (binary (%psection-binary sec)))
    (flet ((node (lines mode)
             (make-tree-file (%join-lines lines) :type :text :mode mode))
           (opaque (kind names mode)
             (make-instance '%parsed-opaque :kind kind :path names
                                            :footprint (list (%tree-fp names))
                                            :description binary :mode mode)))
      (cond
        ((and (%psection-copy-from sec) (%psection-copy-to sec))
         ;; The copy's content is its source's, which the patch does not carry.
         (let ((names (%split-names (%psection-copy-to sec)))
               (from (%split-names (%psection-copy-from sec))))
           (list (make-instance '%parsed-opaque :kind :add :path names :from from
                                                :footprint (list (%tree-fp from) (%tree-fp names))
                                                :description (format nil "copy of ~A"
                                                                     (%psection-copy-from sec))))))
        (added
         (let ((names (%split-names (or new old)))
               (mode (%psection-new-file-mode sec)))
           (list (if binary
                     (opaque :add names mode)
                     (make-change :add :path names :footprint (list (%tree-fp names))
                                       :after (node (mapcan (lambda (h) (copy-list (change-after h)))
                                                            (reverse hunks))
                                                    mode))))))
        (deleted
         (let ((names (%split-names (or old new)))
               (mode (%psection-deleted-file-mode sec)))
           (list (if binary
                     (opaque :remove names mode)
                     (make-change :remove :path names :footprint (list (%tree-fp names))
                                          :before (node (mapcan (lambda (h) (copy-list (change-before h)))
                                                                (reverse hunks))
                                                        mode))))))
        (t
         (let ((names (%split-names (or new old))))
           (append
            (when (and (%psection-rename-from sec) (%psection-rename-to sec))
              (let ((from (%split-names (%psection-rename-from sec))))
                (list (make-change :move :path names :from from
                                         :footprint (list (%tree-fp from) (%tree-fp names))))))
            (when (and (%psection-old-mode sec) (%psection-new-mode sec))
              (list (make-change :modify :path (append names (list 'tree-mode))
                                         :footprint (list (%tree-fp names))
                                         :before (%psection-old-mode sec)
                                         :after (%psection-new-mode sec))))
            (when binary
              (list (opaque :modify names nil)))
            (mapcar (lambda (h) (%nest h names)) hunks))))))))

(defun %git-header-paths (rest)
  "\"a/X b/Y\", either side possibly C-quoted → (values X Y); (values NIL NIL) when
REST is not of that form; :MALFORMED for a bad quoted path."
  (flet ((strip (p prefix)
           (and (%prefix-p prefix p) (subseq p (length prefix)))))
    (cond
      ((%prefix-p "\"" rest)
       (multiple-value-bind (a end) (%c-unquote rest 0)
         (if (and a (< end (length rest)) (char= #\Space (char rest end)))
             (let ((b (%maybe-unquote (subseq rest (1+ end)))))
               (if (eq b :malformed)
                   :malformed
                   (values (strip a "a/") (strip b "b/"))))
             :malformed)))
      ((position #\" rest)
       ;; a/X "b/Y": an unquoted path never holds a double quote.
       (let ((pos (position #\" rest)))
         (multiple-value-bind (b end) (%c-unquote rest pos)
           (if (and b (= end (length rest)) (plusp pos) (char= #\Space (char rest (1- pos))))
               (values (strip (subseq rest 0 (1- pos)) "a/") (strip b "b/"))
               :malformed))))
      (t
       (let ((pos (search " b/" rest :from-end t)))
         (if (and pos (%prefix-p "a/" rest))
             (values (subseq rest 2 pos) (subseq rest (+ pos 3)))
             (values nil nil)))))))

(defun %read-stream-string (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
          while c do (write-char c out))))

(defmethod parse-diff ((format (eql :unified)) input)
  (let* ((text (etypecase input
                 (string input)
                 (stream (%read-stream-string input))))
         (lines (coerce (split-lines text) 'simple-vector))
         (n (length lines))
         (i 0)
         (sections '())
         (current nil)
         (bare '()))
    (flet ((open-section ()
             (setf current (%make-psection))
             (push current sections)))
      (loop while (< i n)
            do (let ((l (%chomp (svref lines i))))
                 (cond
                   ((%prefix-p "diff --git " l)
                    (open-section)
                    (multiple-value-bind (a b) (%git-header-paths (subseq l 11))
                      (when (eq a :malformed) (%parse-error (1+ i) l))
                      (setf (%psection-git-old current) a (%psection-git-new current) b))
                    (incf i))
                   ((and (%prefix-p "--- " l) (< (1+ i) n)
                         (%prefix-p "+++ " (svref lines (1+ i))))
                    (when (or (null current) (%psection-saw-minus current) (%psection-hunks current))
                      (open-section))
                    (let ((old (%strip-path (subseq l 4)))
                          (plus (%chomp (svref lines (1+ i)))))
                      (when (eq old :malformed) (%parse-error (1+ i) l))
                      (let ((new (%strip-path (subseq plus 4))))
                        (when (eq new :malformed) (%parse-error (+ 2 i) plus))
                        (setf (%psection-old-path current) old
                              (%psection-new-path current) new
                              (%psection-saw-minus current) t
                              (%psection-saw-plus current) t)))
                    (incf i 2))
                   ((%prefix-p "@@" l)
                    (multiple-value-bind (hunk next) (%parse-hunk lines i)
                      (if current
                          (push hunk (%psection-hunks current))
                          (push hunk bare))
                      (setf i next)))
                   (t
                    (when current
                      (flet ((after (prefix) (subseq l (length prefix)))
                             (path-after (prefix)
                               (let ((p (%maybe-unquote (subseq l (length prefix)))))
                                 (when (eq p :malformed) (%parse-error (1+ i) l))
                                 p)))
                        (cond ((%prefix-p "rename from " l)
                               (setf (%psection-rename-from current) (path-after "rename from ")))
                              ((%prefix-p "rename to " l)
                               (setf (%psection-rename-to current) (path-after "rename to ")))
                              ((%prefix-p "copy from " l)
                               (setf (%psection-copy-from current) (path-after "copy from ")))
                              ((%prefix-p "copy to " l)
                               (setf (%psection-copy-to current) (path-after "copy to ")))
                              ((%prefix-p "new file mode " l)
                               (setf (%psection-new-file-mode current) (%octal (after "new file mode "))))
                              ((%prefix-p "deleted file mode " l)
                               (setf (%psection-deleted-file-mode current)
                                     (%octal (after "deleted file mode "))))
                              ((%prefix-p "old mode " l)
                               (setf (%psection-old-mode current) (%octal (after "old mode "))))
                              ((%prefix-p "new mode " l)
                               (setf (%psection-new-mode current) (%octal (after "new mode "))))
                              ((%prefix-p "Binary files " l)
                               (setf (%psection-binary current) l)))))
                    (incf i))))))
    (if sections
        (make-diff (%canonical-sort (mapcan #'%section-changes (reverse sections)))
                   :engine (find-diff-engine :tree) :level :changes)
        (make-diff bare :engine (find-diff-engine :text) :level :changes))))
