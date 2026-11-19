(in-package #:diff-protocol)

;;; The sexp / plist engine (brief: "The sexp engine"): classification,
;;; STATE-EQUAL, the content tree shaped per level, and the pure path algebra
;;; that APPLY-DIFF uses through %APPLY-CHANGE / %PUT-VALUE.

;;; Classification

(defun %proper-list-p (x)
  "True when X is a proper list (NIL included). Circular lists are not supported."
  (loop for tail = x then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %plist-p (x)
  "True when X is a non-empty proper list of even length, keywords at even positions,
no duplicate keys."
  (and (consp x)
       (%proper-list-p x)
       (evenp (length x))
       (loop for (k) on x by #'cddr
             for seen = (list k) then (cons k seen)
             always (and (keywordp k) (not (member k (rest seen)))))))

(defun %shape (x)
  "→ :NIL, :PLIST, :LIST or :ATOM."
  (cond ((null x) :nil)
        ((consp x) (if (%plist-p x) :plist :list))
        (t :atom)))

(defun %plist-family-p (sa sb)
  "Both shapes are plists or NIL, at least one a plist."
  (and (member sa '(:nil :plist)) (member sb '(:nil :plist))
       (or (eq sa :plist) (eq sb :plist))))

(defun %list-family-p (sa sb)
  "Both shapes are lists or NIL, at least one a list."
  (and (member sa '(:nil :list)) (member sb '(:nil :list))
       (or (eq sa :list) (eq sb :list))))

(defun %containers-p (a b)
  (let ((sa (%shape a)) (sb (%shape b)))
    (or (%plist-family-p sa sb) (%list-family-p sa sb))))

(defun %check-content (engine x)
  "Signal UNSUPPORTED-CONTENT unless X is sexp content: atoms (numbers, strings,
characters, symbols) and proper lists of sexp content."
  (typecase x
    ((or number string character symbol) nil)
    (cons (unless (%proper-list-p x)
            (error 'unsupported-content :engine engine :content x))
     (dolist (y x) (%check-content engine y)))
    (t (error 'unsupported-content :engine engine :content x))))

(defun %plist-get (plist key)
  "→ (values value found-p). Values may be NIL, so GETF alone is not enough."
  (loop for (k v) on plist by #'cddr
        when (eq k key) do (return-from %plist-get (values v t)))
  (values nil nil))

(defun %plist-has-p (plist key)
  (nth-value 1 (%plist-get plist key)))

;;; Equality

(defmethod state-equal ((e sexp-diff-engine) a b)
  (let ((sa (%shape a)) (sb (%shape b)))
    (cond ((and (null a) (null b)) t)
          ((%plist-family-p sa sb)
           (and (= (length a) (length b))
                (loop for (k va) on a by #'cddr
                      always (multiple-value-bind (vb found) (%plist-get b k)
                               (and found (state-equal e va vb))))))
          ((%list-family-p sa sb)
           (and (= (length a) (length b))
                (every (lambda (x y) (state-equal e x y)) a b)))
          ((or (consp a) (consp b)) nil)
          (t (equal a b)))))

;;; Footprints

(defun %fp (path)
  "Footprint string of PATH: \"sexp:/\" + steps joined by \"/\"."
  (coerce (format nil "sexp:/~{~A~^/~}"
          (mapcar (lambda (s)
                    (if (integerp s) (format nil "~D" s) (string-downcase (symbol-name s))))
                  path))
          '(simple-array character (*))))

;;; Content tree

(defun %node (e kind path from va vb mode)
  "One change of KIND at PATH. MODE :TREE → loaded values, plus children for a
container-vs-container :MODIFY; :SUMMARY → thunks only, no children."
  (let ((footprint (if (eq kind :move) (list (%fp from) (%fp path)) (list (%fp path))))
        (sides (if (eq mode :summary)
                   (append (unless (eq kind :add) (list :before-fn (lambda () va)))
                           (unless (eq kind :remove) (list :after-fn (lambda () vb))))
                   (append (unless (eq kind :add) (list :before va))
                           (unless (eq kind :remove) (list :after vb))))))
    (apply #'make-change kind :path path :from from :footprint footprint
           :children (when (and (eq mode :tree) (eq kind :modify) (%containers-p va vb))
                       (%node-changes e va vb path :tree))
           sides)))

(defun %pair-moves (e a b removed added)
  "Alist removed-key → added-key. Removed keys in A's order each take the first unused
added key (in B's order) whose value is STATE-EQUAL."
  (let ((free (copy-list added))
        (pairs '()))
    (dolist (r removed (nreverse pairs))
      (let* ((va (%plist-get a r))
             (m (find-if (lambda (k) (state-equal e va (%plist-get b k))) free)))
        (when m
          (setf free (remove m free :count 1))
          (push (cons r m) pairs))))))

(defun %plist-changes (e a b path mode)
  "A's key order for modify / remove / move, then B's key order for adds."
  (let* ((removed (loop for (k) on a by #'cddr unless (%plist-has-p b k) collect k))
         (added (loop for (k) on b by #'cddr unless (%plist-has-p a k) collect k))
         (pairs (%pair-moves e a b removed added)))
    (nconc
     (loop for (k va) on a by #'cddr
           for p = (append path (list k))
           nconc (multiple-value-bind (vb found) (%plist-get b k)
                   (let ((to (cdr (assoc k pairs))))
                     (cond (found
                            (unless (state-equal e va vb)
                              (list (%node e :modify p nil va vb mode))))
                           (to
                            (list (%node e :move (append path (list to)) p
                                         va (%plist-get b to) mode)))
                           (t (list (%node e :remove p nil va nil mode)))))))
     (loop for (k vb) on b by #'cddr
           unless (or (%plist-has-p a k) (rassoc k pairs))
             collect (%node e :add (append path (list k)) nil nil vb mode)))))

(defun %list-changes (e a b path mode)
  "Common-index modifies ascending, tail removes highest index first, tail adds lowest first."
  (let ((la (length a)) (lb (length b)))
    (nconc (loop for i from 0
                 for va in a
                 for vb in b
                 unless (state-equal e va vb)
                   collect (%node e :modify (append path (list i)) nil va vb mode))
           (loop for i from (1- la) downto lb
                 collect (%node e :remove (append path (list i)) nil (nth i a) nil mode))
           (loop for i from la below lb
                 collect (%node e :add (append path (list i)) nil nil (nth i b) mode)))))

(defun %node-changes (e a b path mode)
  "The changes that turn A into B at absolute PATH, in apply order."
  (let ((sa (%shape a)) (sb (%shape b)))
    (cond ((state-equal e a b) '())
          ((%plist-family-p sa sb) (%plist-changes e a b path mode))
          ((%list-family-p sa sb) (%list-changes e a b path mode))
          (t (list (%node e :modify path nil a b mode))))))

(defmethod compute-diff ((e sexp-diff-engine) a b &key (level :changes))
  (check-type level diff-level)
  (%check-content e a)
  (%check-content e b)
  (let ((roots (%node-changes e a b '() (if (eq level :summary) :summary :tree))))
    (make-diff (if (eq level :changes) (%leaves roots) roots)
               :engine e :level level)))

;;; Pure path algebra. New structure is allocated only along the written path.

(defun %step-get (state step)
  "→ (values child found-p) for one path STEP."
  (cond ((keywordp step)
         (if (member (%shape state) '(:nil :plist))
             (%plist-get state step)
             (values nil nil)))
        ((and (integerp step) (>= step 0) (consp state) (%proper-list-p state)
              (< step (length state)))
         (values (nth step state) t))
        (t (values nil nil))))

(defun %state-get (state path)
  "→ (values value found-p) at PATH. The root is always found."
  (if (null path)
      (values state t)
      (multiple-value-bind (child found) (%step-get state (first path))
        (if found
            (%state-get child (rest path))
            (values nil nil)))))

(defun %plist-set (plist key value)
  "Fresh plist: KEY replaced in place, or appended at the end."
  (if (%plist-has-p plist key)
      (loop for (k v) on plist by #'cddr
            nconc (list k (if (eq k key) value v)))
      (append plist (list key value))))

(defun %state-put (state path value fail)
  "→ new state with VALUE at PATH. A missing keyword parent is created; an
unreachable list index calls FAIL."
  (if (null path)
      value
      (let ((step (first path)))
        (cond ((keywordp step)
               (let ((plist (if (member (%shape state) '(:nil :plist)) state nil)))
                 (%plist-set plist step
                             (%state-put (%plist-get plist step) (rest path) value fail))))
              ((and (integerp step) (listp state) (%proper-list-p state)
                    (<= 0 step (length state)))
               (let ((len (length state)))
                 (if (= step len)
                     (if (rest path)
                         (funcall fail)
                         (append state (list value)))
                     (append (subseq state 0 step)
                             (list (%state-put (nth step state) (rest path) value fail))
                             (nthcdr (1+ step) state)))))
              (t (funcall fail))))))

(defun %state-delete (state path)
  "→ new state without the entry at PATH (which must be present)."
  (let ((step (first path)))
    (if (rest path)
        (%state-put state (list step)
                    (%state-delete (%step-get state step) (rest path))
                    (lambda () (error "Internal: delete path vanished")))
        (if (keywordp step)
            (loop for (k v) on state by #'cddr
                  unless (eq k step) nconc (list k v))
            (append (subseq state 0 step) (nthcdr (1+ step) state))))))

;;; Apply one leaf

(defmethod apply-change ((e sexp-diff-engine) state (c opaque-change) d)
  (declare (ignore state))
  (error 'unsupported-content :engine e :content c :change c :diff d))

(defmethod apply-change ((e sexp-diff-engine) state (c change) d)
  (labels ((fail (reason actual)
             (error 'apply-conflict :reason reason :actual actual :change c :diff d))
           (expect-before (p)
             (multiple-value-bind (v found) (%state-get state p)
               (cond ((not found) (fail :missing nil))
                     ((not (state-equal e v (change-before c))) (fail :before-mismatch v)))))
           (expect-absent (p)
             (multiple-value-bind (v found) (%state-get state p)
               (when found (fail :exists v)))
             (multiple-value-bind (parent pfound) (%state-get state (butlast p))
               (let ((step (car (last p))))
                 (unless (and pfound
                              (if (keywordp step)
                                  (member (%shape parent) '(:nil :plist))
                                  (and (listp parent) (%proper-list-p parent)
                                       (eql step (length parent)))))
                   (fail :missing nil)))))
           (put (s p v)
             (%state-put s p v (lambda () (fail :missing nil)))))
    (let ((p (change-path c)))
      (ecase (change-kind c)
        (:modify (expect-before p) (put state p (change-after c)))
        (:remove (expect-before p) (%state-delete state p))
        (:add (expect-absent p) (put state p (change-after c)))
        (:move (expect-before (change-from c))
         (expect-absent p)
         (put (%state-delete state (change-from c)) p (change-after c)))))))

(defmethod put-value ((e sexp-diff-engine) state (c change) value d)
  (%state-put state (change-path c) value
              (lambda ()
                (error 'apply-conflict :reason :missing :actual nil :change c :diff d))))
