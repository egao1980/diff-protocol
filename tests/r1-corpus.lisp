(in-package #:diff-protocol/tests)

;;; R1 corpora and helpers: texts, trees, a deterministic random generator,
;;; git fixtures. Everything that calls R1 code (make-tree, compute-diff on the
;;; text / tree engines) runs inside test bodies: rows are forms, evaluated when
;;; the test runs, so the test system loads while R1 is unimplemented.

(defvar *text* (make-instance 'd:text-diff-engine))
(defvar *histogram* (make-instance 'd:text-diff-engine :algorithm :histogram))
(defvar *tree* (make-instance 'd:tree-diff-engine))

(defparameter *text-engines* '(*text* *histogram*)
  "Symbols (rows splice them as forms, so they evaluate to the engines).")

(defun tx= (a b) (d:state-equal *text* a b))
(defun tr= (a b) (d:state-equal *tree* a b))

(defun cat (&rest strings)
  "Concatenate STRINGS; (cat) → \"\"."
  (apply #'concatenate 'string strings))

(defun crlf (&rest lines)
  "Each of LINES followed by CR LF."
  (apply #'cat (mapcar (lambda (l) (cat l (string #\Return) (string #\Newline))) lines)))

(defun nl (&rest lines)
  "Each of LINES followed by LF."
  (apply #'cat (mapcar (lambda (l) (cat l (string #\Newline))) lines)))

(defun lines-of (string)
  "Test-local line split (independent of D:SPLIT-LINES): lines keep their #\\Newline;
a last line without one is kept; \"\" → NIL."
  (loop with start = 0
        with n = (length string)
        while (< start n)
        collect (let ((pos (position #\Newline string :start start)))
                  (prog1 (subseq string start (if pos (1+ pos) n))
                    (setf start (if pos (1+ pos) n))))))

(defun lcs-length (xs ys)
  "Length of a longest common subsequence of line lists XS and YS (STRING=)."
  (let* ((a (coerce xs 'vector)) (b (coerce ys 'vector))
         (m (length a)) (n (length b))
         (row (make-array (1+ n) :initial-element 0)))
    (dotimes (i m (aref row n))
      (let ((diag 0))
        (loop for j from 1 to n
              do (let ((up (aref row j)))
                   (setf (aref row j)
                         (if (string= (aref a i) (aref b (1- j)))
                             (1+ diag)
                             (max up (aref row (1- j)))))
                   (setf diag up)))))))

(defun text-fp (start end &optional (name ""))
  (format nil "text:~A/~D-~D" name start end))

;;; Fixed text corpus (brief: tricky texts)

(defparameter *texts*
  (list (list :empty "")
        (list :one (nl "a"))
        (list :one-no-eol "a")
        (list :abc (nl "a" "b" "c"))
        (list :abc-no-eol (cat (nl "a" "b") "c"))
        (list :abc-crlf (crlf "a" "b" "c"))
        (list :mixed-eol (cat (crlf "a") (nl "b") (crlf "c")))
        (list :xyz (nl "x" "y" "z"))
        (list :abcd-mod (nl "a" "B" "c" "d"))
        (list :blank (nl "" "" ""))
        (list :repeats (nl "a" "a" "a" "b" "a"))
        (list :code (nl "(defun f (x)" "  (+ x 1))" "" "(defun g (y)" "  (* y 2))")))
  "Name → string. Covers empty, no trailing newline, CRLF and mixed line ends, identical
and all-different pairs (every ordered pair is used), blank lines and repeated lines.")

(defun text (name)
  (let ((entry (assoc name *texts*)))
    (assert entry () "No corpus text ~S" name)
    (second entry)))

;;; Deterministic pseudo-random generator (portable LCG; not CL:RANDOM, whose
;;; sequence is implementation-specific).

(defun make-rng (seed)
  "→ function (N) → integer in [0, N)."
  (let ((s seed))
    (lambda (n)
      (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
      (mod (floor s 65536) n))))

(defparameter *line-pool*
  (list (nl "a") (nl "b") (nl "c") (nl "a") (crlf "d") (nl "")))

(defun random-text (rng)
  (let ((lines (loop repeat (funcall rng 8)
                     collect (nth (funcall rng (length *line-pool*)) *line-pool*))))
    (apply #'cat (append lines (when (zerop (funcall rng 4)) (list "z"))))))

(defun mutate-text (rng text)
  "Random line edits of TEXT: keep, replace, delete, insert after."
  (let ((out '()))
    (dolist (line (lines-of text))
      (let ((r (funcall rng 20)))
        (cond ((< r 13) (push line out))
              ((< r 16) (push (nth (funcall rng (length *line-pool*)) *line-pool*) out))
              ((< r 18) nil)
              (t (push line out)
                 (push (nth (funcall rng (length *line-pool*)) *line-pool*) out)))))
    (when (zerop (funcall rng 5))
      (push (nl "new") out))
    ;; A line without a newline may only end the text.
    (let ((lines (nreverse out)))
      (apply #'cat (loop for (l . more) on lines
                         collect (if (and more (not (char= #\Newline (char l (1- (length l))))))
                                     (cat l (string #\Newline))
                                     l))))))

(defparameter *random-text-pairs*
  (let ((rng (make-rng 20261119)))
    (loop repeat 40
          collect (let ((a (random-text rng)))
                    (list a (if (zerop (funcall rng 3)) (random-text rng) (mutate-text rng a)))))))

(defparameter *random-text-triples*
  (let ((rng (make-rng 1980)))
    (loop repeat 40
          collect (let ((base (random-text rng)))
                    (list base (mutate-text rng base) (mutate-text rng base))))))

(defun text-pair-rows (&key (engines *text-engines*) (levels *levels*))
  "Rows (engine a b level) over all ordered corpus pairs and the random pairs."
  (loop for engine in engines
        nconc (loop for level in levels
                    nconc (loop for (nil a) in *texts*
                                nconc (loop for (nil b) in *texts*
                                            collect (list engine a b level)))
                    nconc (loop for (a b) in *random-text-pairs*
                                collect (list engine a b level)))))

(defun text-pair-ids (&key (engines *text-engines*) (levels *levels*))
  (loop for engine in engines
        nconc (loop for level in levels
                    nconc (loop for (x) in *texts*
                                nconc (loop for (y) in *texts*
                                            collect (format nil "~(~A:~A>~A@~A~)"
                                                            engine x y level)))
                    nconc (loop for i from 0 below (length *random-text-pairs*)
                                collect (format nil "~(~A:random-~D@~A~)" engine i level)))))

(defun text-triple-rows (&key (engines *text-engines*))
  "Rows (engine a b c): corpus triples plus the random triples."
  (loop for engine in engines
        nconc (loop for (x y z) in '((:abc :abcd-mod :xyz) (:empty :abc :abc-no-eol)
                                     (:abc-crlf :mixed-eol :abc) (:repeats :blank :code)
                                     (:one :one-no-eol :empty))
                    collect (list engine (text x) (text y) (text z)))
        nconc (loop for triple in *random-text-triples* collect (cons engine triple))))

(defun text-triple-ids (&key (engines *text-engines*))
  (loop for engine in engines
        nconc (loop for (x y z) in '((:abc :abcd-mod :xyz) (:empty :abc :abc-no-eol)
                                     (:abc-crlf :mixed-eol :abc) (:repeats :blank :code)
                                     (:one :one-no-eol :empty))
                    collect (format nil "~(~A:~A>~A>~A~)" engine x y z))
        nconc (loop for i from 0 below (length *random-text-triples*)
                    collect (format nil "~(~A:random-~D~)" engine i))))

;;; Trees. Entries: (path-string content &rest make-tree-file-keys).

(defun split-path (string)
  "\"a/b/c\" → (\"a\" \"b\" \"c\")."
  (loop with start = 0
        for pos = (position #\/ string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun tr (entries)
  "→ a tree (via D:MAKE-TREE / D:MAKE-TREE-FILE) from ENTRIES, a list of
(path content &key type mode). Paths are \"/\"-separated."
  (labels ((build (items)
             ;; ITEMS: list of (components content . keys)
             (let ((names '()))
               (dolist (it items)
                 (pushnew (first (first it)) names :test #'string=))
               (apply #'d:make-tree
                      (loop for name in (reverse names)
                            for mine = (remove-if-not (lambda (it) (string= name (first (first it))))
                                                      items)
                            for leaf = (find-if (lambda (it) (null (rest (first it)))) mine)
                            nconc (list name
                                        (if leaf
                                            (apply #'d:make-tree-file (second leaf) (cddr leaf))
                                            (build (mapcar (lambda (it) (cons (rest (first it)) (rest it)))
                                                           mine)))))))))
    (build (mapcar (lambda (e) (cons (split-path (first e)) (rest e))) entries))))

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defparameter *trees*
  `((:empty ())
    (:one (("a" ,(nl "x"))))
    (:nested (("README" ,(nl "hi")) ("src/x.lisp" ,(nl "(a)" "(b)" "(c)" "(d)"))
              ("src/y.lisp" ,(nl "(y)"))))
    (:nested-edit (("README" ,(nl "hi" "there")) ("src/x.lisp" ,(nl "(a)" "(B)" "(c)" "(d)"))
                   ("src/y.lisp" ,(nl "(y)"))))
    (:renamed (("README" ,(nl "hi")) ("lib/x.lisp" ,(nl "(a)" "(b)" "(c)" "(d)"))
               ("src/y.lisp" ,(nl "(y)"))))
    (:renamed-edit (("README" ,(nl "hi")) ("lib/x.lisp" ,(nl "(a)" "(b)" "(c)" "(D)"))
                    ("src/y.lisp" ,(nl "(y)"))))
    (:mode (("README" ,(nl "hi")) ("src/x.lisp" ,(nl "(a)" "(b)" "(c)" "(d)") :mode #o100755)
            ("src/y.lisp" ,(nl "(y)"))))
    (:typed (("cfg" (:a 1 :b (1 2)) :type :sexp) ("logo" ,(octets 1 2 3)) ("t" "no-eol")))
    (:typed-edit (("cfg" (:a 2 :b (1 2 3)) :type :sexp) ("logo" ,(octets 1 2 4)) ("t" ,(nl "no-eol"))))
    (:dir-to-file (("src" ,(nl "now a file")) ("README" ,(nl "hi"))))
    (:crlf (("w.txt" ,(crlf "a" "b")) ("empty" ""))))
  "Name → tree entries (plain data; turned into trees by TR at test time).")

(defun tree-entries-of (name)
  (let ((entry (assoc name *trees*)))
    (assert entry () "No corpus tree ~S" name)
    (second entry)))

(defun tree-form (entries) (list 'tr (q entries)))

(defparameter *tree-paths* '("a" "b" "c" "c/x" "d/e" "d/f" "g/h/i" "README"))
(defparameter *tree-contents*
  (list (nl "x") (nl "x" "y") (nl "1" "2" "3" "4") (nl "1" "2" "3" "X") "" "q" (crlf "w")))

(defun random-tree-entries (rng)
  "A random set of files: paths from *TREE-PATHS* (skipping file/dir collisions),
contents from *TREE-CONTENTS* (duplicates on purpose, so renames occur)."
  (let ((out '()))
    (dolist (p *tree-paths* (nreverse out))
      (when (and (< (funcall rng 10) 6)
                 (notany (lambda (e)
                           (let ((q (first e)))
                             (or (and (< (length q) (length p))
                                      (string= (cat q "/") p :end2 (1+ (length q))))
                                 (and (< (length p) (length q))
                                      (string= (cat p "/") q :end2 (1+ (length p)))))))
                         out))
        (push (list p (nth (funcall rng (length *tree-contents*)) *tree-contents*)) out)))))

(defparameter *random-tree-pairs*
  (let ((rng (make-rng 7)))
    (loop repeat 25 collect (list (random-tree-entries rng) (random-tree-entries rng)))))

(defparameter *tree-pairs*
  '((:empty :one) (:one :empty) (:nested :nested-edit) (:nested-edit :nested) (:nested :renamed)
    (:renamed :nested) (:nested :renamed-edit) (:nested :mode) (:mode :nested) (:typed :typed-edit)
    (:typed-edit :typed) (:nested :dir-to-file) (:dir-to-file :nested) (:crlf :one) (:typed :nested)))

(defun tree-pair-rows ()
  (loop for level in *levels*
        nconc (loop for (x y) in *tree-pairs*
                    collect (list (tree-form (tree-entries-of x)) (tree-form (tree-entries-of y)) level))
        nconc (loop for (ea eb) in *random-tree-pairs*
                    collect (list (tree-form ea) (tree-form eb) level))))

(defun tree-pair-ids ()
  (loop for level in *levels*
        nconc (loop for (x y) in *tree-pairs* collect (format nil "~(~A>~A@~A~)" x y level))
        nconc (loop for i from 0 below (length *random-tree-pairs*)
                    collect (format nil "~(random-~D@~A~)" i level))))

(defparameter *random-tree-triples*
  (let ((rng (make-rng 42)))
    (loop repeat 25
          collect (let ((base (random-tree-entries rng)))
                    (flet ((edit ()
                             ;; Mostly base, with some files changed, dropped or added.
                             (let ((out (loop for e in base
                                              for r = (funcall rng 10)
                                              unless (< r 2)
                                                collect (if (< r 4)
                                                            (list (first e)
                                                                  (nth (funcall rng (length *tree-contents*))
                                                                       *tree-contents*))
                                                            e))))
                               (if (zerop (funcall rng 3))
                                   (let ((extra (random-tree-entries rng)))
                                     (append out (remove-if (lambda (x)
                                                              (some (lambda (y)
                                                                      (let ((p (first x)) (q (first y)))
                                                                        (or (string= p q)
                                                                            (and (< (length q) (length p))
                                                                                 (string= (cat q "/") p :end2 (1+ (length q))))
                                                                            (and (< (length p) (length q))
                                                                                 (string= (cat p "/") q :end2 (1+ (length p)))))))
                                                                    out))
                                                            extra)))
                                   out))))
                      (list base (edit) (edit)))))))

(defun tree-triple-rows ()
  (loop for (b o th) in *random-tree-triples*
        collect (list (tree-form b) (tree-form o) (tree-form th))))

(defun tree-triple-ids ()
  (loop for i from 0 below (length *random-tree-triples*) collect (format nil "random-~D" i)))

;;; Git fixtures (tests/fixtures/git; regenerate with tests/fixtures/make-git-fixtures.sh)

(defun fixture-path (relative)
  (asdf:system-relative-pathname "diff-protocol" (cat "tests/fixtures/" relative)))

(defun read-file-string (pathname)
  "Whole file as a string, byte for byte (LATIN-1: CR and every byte kept)."
  (with-open-file (in pathname :external-format :latin-1)
    (let* ((s (make-string (file-length in)))
           (n (read-sequence s in)))
      (subseq s 0 n))))

(defun read-file-octets (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let* ((v (make-array (file-length in) :element-type '(unsigned-byte 8)))
           (n (read-sequence v in)))
      (subseq v 0 n))))

(defun fixture-entries (case side &key modes binary)
  "Tree entries for tests/fixtures/git/CASE/SIDE/. MODES: alist path → mode;
BINARY: list of paths read as octets (type :binary)."
  (let* ((root (truename (fixture-path (format nil "git/~A/~A/" case side))))
         (files (remove-if-not #'pathname-name (directory (merge-pathnames "**/*.*" root)))))
    (sort (loop for f in files
                for rel = (enough-namestring f root)
                for mode = (cdr (assoc rel modes :test #'string=))
                collect (append (list rel (if (member rel binary :test #'string=)
                                              (read-file-octets f)
                                              (read-file-string f)))
                                (when mode (list :mode mode))))
          #'string< :key #'first)))

(defun fixture-diff-text (case)
  (read-file-string (fixture-path (format nil "git/~A.diff" case))))
