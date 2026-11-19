(in-package #:diff-protocol)

;;; R1 tree engine (brief: "Tree engine"). The tree engine is a facade over the
;;; leaf engines: file-level changes are its own; a file's content changes come
;;; from the leaf engine and are relocated (%NEST / %UNNEST: paths prefixed with
;;; the file's names, footprints spliced, lazy boxes shared).

(defclass tree-diff-engine (diff-engine)
  ((rename-threshold :initarg :rename-threshold :initform 0.5
                     :reader tree-engine-rename-threshold)
   (detect-renames :initarg :detect-renames :initform t
                   :reader tree-engine-detect-renames)
   (leaf-engines :initarg :leaf-engines :initform '((:text . :text) (:sexp . :sexp))
                 :reader tree-engine-leaf-engines))
  (:documentation "Engine for tree values (MAKE-TREE). ENGINE-NAME → :TREE.
LEAF-ENGINES: alist file type → engine name (via FIND-DIFF-ENGINE) or engine instance.
RENAME-THRESHOLD: line similarity in [0, 1] at or above which a removed and an added
text file pair up as a :MOVE. DETECT-RENAMES NIL turns rename detection off."))

(defmethod engine-name ((e tree-diff-engine))
  :tree)

(register-diff-engine :tree (make-instance 'tree-diff-engine))

;;; Paths and footprints

(defun %path-string (names)
  (format nil "~{~A~^/~}" names))

(defun %tree-fp (names)
  (concatenate 'string "tree:/" (%path-string names)))

(defun %splice-fp (f names)
  "Leaf footprint F with the file path NAMES spliced in after the scheme:
\"text:/3-5\" → \"text:/src/x.lisp/3-5\", \"sexp:/\" → \"sexp:/cfg\"."
  (let ((colon (position #\: f)))
    (if (and colon (< (1+ colon) (length f)) (char= #\/ (char f (1+ colon))))
        (let ((rest (subseq f (1+ colon))))
          (concatenate 'string (subseq f 0 (1+ colon)) "/" (%path-string names)
                       (if (string= rest "/") "" rest)))
        f)))

(defun %tree-names (path)
  "The leading strings of PATH: the file part."
  (loop for step in path while (stringp step) collect step))

(defun %leaf-change-p (c)
  (notevery #'stringp (change-path c)))

(defun %mode-change-p (c)
  (eq 'tree-mode (car (last (change-path c)))))

(defun %side-available-p (box)
  "True when a side has content: a value or a thunk (a parsed rename has neither)."
  (or (%lazy-loaded-p box) (%lazy-fn box)))

(defun %nest (c names)
  "Leaf engine change C → tree change under the file NAMES (boxes shared)."
  (make-instance 'change
                 :kind (change-kind c)
                 :path (append names (change-path c))
                 :from (and (change-from c) (append names (change-from c)))
                 :footprint (cons (%tree-fp names)
                                  (mapcar (lambda (f) (%splice-fp f names)) (change-footprint c)))
                 :before-box (%before-box c)
                 :after-box (%after-box c)
                 :children (mapcar (lambda (x) (%nest x names)) (change-children c))))

(defun %unnest (c n)
  "Tree leaf change C → the leaf engine's change (N leading names dropped)."
  (make-instance 'change
                 :kind (change-kind c)
                 :path (nthcdr n (change-path c))
                 :from (and (change-from c) (nthcdr n (change-from c)))
                 :footprint (change-footprint c)
                 :before-box (%before-box c)
                 :after-box (%after-box c)))

;;; Leaf engines and equality

(defun %leaf-engine (e type)
  (let ((spec (cdr (assoc type (tree-engine-leaf-engines e)))))
    (if (symbolp spec) (and spec (find-diff-engine spec)) spec)))

(defun %content-equal (e type a b)
  (let ((leaf (%leaf-engine e type)))
    (if leaf (state-equal leaf a b) (equalp a b))))

(defun %file-equal (e a b)
  (and (tree-file-p a) (tree-file-p b)
       (eq (tree-file-type a) (tree-file-type b))
       (eql (%file-mode a) (%file-mode b))
       (%content-equal e (tree-file-type a) (tree-file-content a) (tree-file-content b))))

(defun %sorted-files (dir)
  (sort (%tree-files dir) #'string< :key (lambda (x) (%path-string (car x)))))

(defmethod state-equal ((e tree-diff-engine) a b)
  (cond ((and (tree-dir-p a) (tree-dir-p b))
         (let ((fa (%tree-files a))
               (fb (%tree-files b)))
           (and (= (length fa) (length fb))
                (every (lambda (x)
                         (let ((y (assoc (car x) fb :test #'equal)))
                           (and y (%file-equal e (cdr x) (cdr y)))))
                       fa))))
        ((and (tree-file-p a) (tree-file-p b)) (%file-equal e a b))
        (t (equalp a b))))

;;; Rename detection

(defun %empty-content-p (c)
  (or (null c) (and (vectorp c) (zerop (length c)))))

(defun %similarity (a b)
  "Line similarity 2·LCS / (lines(a) + lines(b)) of two strings, a rational."
  (let ((av (%line-vector a))
        (bv (%line-vector b)))
    (if (zerop (+ (length av) (length bv)))
        1
        (/ (* 2 (%lcs-length av bv)) (+ (length av) (length bv))))))

(defun %detect-renames (e removed added)
  "→ list of (removed-entry . added-entry); entries are (names . file)."
  (let ((free-r (remove-if (lambda (x) (%empty-content-p (tree-file-content (cdr x)))) removed))
        (free-a (remove-if (lambda (x) (%empty-content-p (tree-file-content (cdr x)))) added))
        (pairs '()))
    ;; Exact: removed in path order, each takes the first unused added file.
    (dolist (r free-r)
      (let* ((rf (cdr r))
             (m (find-if (lambda (x)
                           (let ((xf (cdr x)))
                             (and (eq (tree-file-type rf) (tree-file-type xf))
                                  (%content-equal e (tree-file-type rf)
                                                  (tree-file-content rf) (tree-file-content xf)))))
                         free-a)))
        (when m
          (push (cons r m) pairs)
          (setf free-a (remove m free-a)))))
    (setf free-r (remove-if (lambda (r) (assoc r pairs)) free-r))
    ;; Similarity, text files only: highest first, ties by removed then added path.
    (let ((candidates '())
          (threshold (tree-engine-rename-threshold e)))
      (dolist (r free-r)
        (when (and (eq :text (tree-file-type (cdr r))) (stringp (tree-file-content (cdr r))))
          (dolist (x free-a)
            (when (and (eq :text (tree-file-type (cdr x))) (stringp (tree-file-content (cdr x))))
              (let ((s (%similarity (tree-file-content (cdr r)) (tree-file-content (cdr x)))))
                (when (>= s threshold)
                  (push (list s r x) candidates)))))))
      (dolist (c (stable-sort (nreverse candidates) #'> :key #'first))
        (destructuring-bind (s r x) c
          (declare (ignore s))
          (when (and (member r free-r) (member x free-a))
            (push (cons r x) pairs)
            (setf free-r (remove r free-r)
                  free-a (remove x free-a))))))
    (nreverse pairs)))

;;; Canonical order

(defun %canonical-rank (c)
  (if (%leaf-change-p c)
      2
      (ecase (change-kind c) (:remove 0) (:move 1) (:modify 2) (:add 3))))

(defun %canonical< (x y)
  (let ((rx (%canonical-rank x))
        (ry (%canonical-rank y)))
    (if (/= rx ry)
        (< rx ry)
        (let ((px (%path-string (%tree-names (change-path x))))
              (py (%path-string (%tree-names (change-path y)))))
          (cond ((string< px py) t)
                ((string< py px) nil)
                (t (and (%mode-change-p x) (not (%mode-change-p y)))))))))

(defun %canonical-sort (changes)
  (stable-sort (copy-list changes) #'%canonical<))

;;; compute-diff

(defun %file-change (kind path from before after lazy &optional children)
  (let ((footprint (if (eq kind :move)
                       (list (%tree-fp from) (%tree-fp path))
                       (list (%tree-fp path)))))
    (apply #'make-change kind :path path :from from :footprint footprint :children children
           (append (unless (eq kind :add)
                     (if lazy (list :before-fn (lambda () before)) (list :before before)))
                   (unless (eq kind :remove)
                     (if lazy (list :after-fn (lambda () after)) (list :after after)))))))

(defun %file-leaves (e names a b context)
  "Children of a file :MODIFY: the mode change, then the leaf engine's changes; NIL when
the types differ or the type has no leaf engine (the whole node changes)."
  (let ((leaf (and (eq (tree-file-type a) (tree-file-type b))
                   (%leaf-engine e (tree-file-type a)))))
    (when leaf
      (let ((ma (%file-mode a))
            (mb (%file-mode b)))
        (append (unless (eql ma mb)
                  (list (make-change :modify :path (append names (list 'tree-mode))
                                             :footprint (list (%tree-fp names))
                                             :before ma :after mb)))
                (mapcar (lambda (x) (%nest x names))
                        (diff-changes (compute-diff leaf (tree-file-content a) (tree-file-content b)
                                                    :level :changes :context context
                                                    :allow-other-keys t))))))))

(defmethod compute-diff ((e tree-diff-engine) a b &key (level :changes) (context 0))
  (check-type level diff-level)
  (unless (tree-dir-p a) (error 'unsupported-content :engine e :content a))
  (unless (tree-dir-p b) (error 'unsupported-content :engine e :content b))
  (let* ((fa (%sorted-files a))
         (fb (%sorted-files b))
         (removed (remove-if (lambda (x) (assoc (car x) fb :test #'equal)) fa))
         (added (remove-if (lambda (x) (assoc (car x) fa :test #'equal)) fb))
         (common (loop for x in fa
                       for y = (assoc (car x) fb :test #'equal)
                       when (and y (not (%file-equal e (cdr x) (cdr y))))
                         collect (list (car x) (cdr x) (cdr y))))
         (moves (when (tree-engine-detect-renames e) (%detect-renames e removed added)))
         (removed (remove-if (lambda (x) (assoc x moves)) removed))
         (added (remove-if (lambda (x) (rassoc x moves)) added))
         (moves (sort (copy-list moves) #'string< :key (lambda (m) (%path-string (cadr m)))))
         (lazy (eq level :summary))
         (changes
           (append
            (mapcar (lambda (x) (%file-change :remove (car x) nil (cdr x) nil lazy)) removed)
            (mapcar (lambda (m) (%file-change :move (cadr m) (caar m) (cdar m) (cddr m) lazy))
                    moves)
            (mapcar (lambda (c)
                      (destructuring-bind (names fa fb) c
                        (%file-change :modify names nil fa fb lazy
                                      (unless lazy (%file-leaves e names fa fb context)))))
                    common)
            (mapcar (lambda (x) (%file-change :add (car x) nil nil (cdr x) lazy)) added))))
    (make-diff (if (eq level :changes) (%leaves changes) changes)
               :engine e :level level)))

;;; Apply

(defmethod apply-change ((e tree-diff-engine) state (c opaque-change) d)
  (declare (ignore state))
  ;; Recoverable through APPLY-DIFF's per-change restarts: USE-VALUE (a file node, put
  ;; at the change's path by %PUT-VALUE) and SKIP-CHANGE.
  (error 'unsupported-content :engine e :content c :change c :diff d))

(defmethod apply-change ((e tree-diff-engine) state (c change) d)
  (unless (tree-dir-p state)
    (error 'unsupported-content :engine e :content state :change c :diff d))
  (let* ((path (change-path c))
         (names (%tree-names path))
         (steps (nthcdr (length names) path)))
    (labels ((fail (reason actual)
               (error 'apply-conflict :reason reason :actual actual :change c :diff d))
             (need-file (s ns)
               (multiple-value-bind (node found) (tree-get s ns)
                 (if (and found ns (tree-file-p node))
                     node
                     (fail :missing (and found node)))))
             (check-before (node)
               (unless (%file-equal e node (change-before c))
                 (fail :before-mismatch node)))
             (check-absent (s ns)
               (loop for i from 1 below (length ns)
                     for node = (tree-get s (subseq ns 0 i))
                     when (tree-file-p node) do (fail :exists node))
               (multiple-value-bind (node found) (tree-get s ns)
                 (when found (fail :exists node))))
             (put (s ns node)
               (%tree-put s ns node (lambda (f) (fail :exists f)))))
      (cond
        ((null names) (fail :missing nil))
        ((null steps)
         (ecase (change-kind c)
           (:add (check-absent state names)
            (put state names (change-after c)))
           (:remove (check-before (need-file state names))
            (%tree-delete state names))
           (:modify (check-before (need-file state names))
            (put state names (change-after c)))
           (:move
            (let ((from (change-from c)))
              (let ((node (need-file state from)))
                (when (%side-available-p (%before-box c))
                  (check-before node))
                (let ((s (%tree-delete state from)))
                  (check-absent s names)
                  (put s names (if (%side-available-p (%after-box c))
                                   (change-after c)
                                   node))))))))
        ((equal steps '(tree-mode))
         (let ((node (need-file state names)))
           (unless (eql (%file-mode node) (or (change-before c) #o100644))
             (fail :before-mismatch (%file-mode node)))
           (put state names (%file-with node :mode (change-after c)))))
        (t
         (let* ((node (need-file state names))
                (leaf (%leaf-engine e (tree-file-type node))))
           (unless leaf
             (error 'unsupported-content :engine e :content node :change c :diff d))
           (let ((content (handler-case
                              (apply-change leaf (tree-file-content node)
                                             (%unnest c (length names)) d)
                            (apply-conflict (x)
                              (fail (apply-conflict-reason x) (apply-conflict-actual x))))))
             (put state names (%file-with node :content content)))))))))

(defmethod put-value ((e tree-diff-engine) state (c change) value d)
  (let* ((path (change-path c))
         (names (%tree-names path))
         (steps (nthcdr (length names) path)))
    (flet ((fail ()
             (error 'apply-conflict :reason :missing :actual nil :change c :diff d)))
      (unless (and (tree-dir-p state) names)
        (fail))
      (if (null steps)
          (%tree-put state names value (lambda (f) (declare (ignore f)) (fail)))
          (multiple-value-bind (node found) (tree-get state names)
            (unless (and found (tree-file-p node))
              (fail))
            (%tree-put state names
                       (if (equal steps '(tree-mode))
                           (%file-with node :mode value)
                           (let ((leaf (%leaf-engine e (tree-file-type node))))
                             (unless leaf (fail))
                             (%file-with node :content
                                         (put-value leaf (tree-file-content node)
                                                     (%unnest c (length names)) value d))))
                       (lambda (f) (declare (ignore f)) (fail))))))))

;;; Three-way and rebase

(defun %file-merge-pair-p (o x)
  "Both sides change the content / mode of the same file, same type, with a leaf engine."
  (and (eq :modify (change-kind o)) (eq :modify (change-kind x))
       (not (typep o 'opaque-change)) (not (typep x 'opaque-change))
       (equal (change-path o) (change-path x))
       (change-children o) (change-children x)))

(defun %merge-file (e o x resolve)
  "→ (values leaf-changes conflicts) for one file both sides modified. The mode and the
content merge independently; the content through the leaf engine's three-way."
  (let* ((names (change-path o))
         (mode-o (find-if #'%mode-change-p (change-children o)))
         (mode-x (find-if #'%mode-change-p (change-children x)))
         (leaves-o (remove-if #'%mode-change-p (change-children o)))
         (leaves-x (remove-if #'%mode-change-p (change-children x)))
         (mode '())
         (leaves '())
         (conflicts '()))
    (cond ((and mode-o mode-x)
           (if (eql (change-after mode-o) (change-after mode-x))
               (setf mode (list mode-o))
               (progn (push (%conflict mode-o mode-x) conflicts)
                      (when (eq resolve :theirs) (setf mode (list mode-x))))))
          (mode-o (setf mode (list mode-o)))
          (mode-x (setf mode (list mode-x))))
    (cond ((and leaves-o leaves-x)
           (let* ((before (change-before o))
                  (leaf (%leaf-engine e (tree-file-type before))))
             (multiple-value-bind (merged cs)
                 (merge-states leaf (tree-file-content before)
                             (tree-file-content (change-after o))
                             (tree-file-content (change-after x))
                             resolve)
               (setf leaves (mapcar (lambda (c) (%nest c names)) (diff-changes merged)))
               (dolist (c cs)
                 (push (%conflict (%nest (conflict-ours c) names) (%nest (conflict-theirs c) names))
                       conflicts)))))
          (leaves-o (setf leaves leaves-o))
          (leaves-x (setf leaves leaves-x)))
    (values (append mode leaves) (nreverse conflicts))))

(defmethod merge-states ((e tree-diff-engine) base ours theirs resolve)
  (let ((os (diff-changes (compute-diff e base ours :level :content)))
        (ts (diff-changes (compute-diff e base theirs :level :content)))
        (merges (make-hash-table :test #'eq))
        (merged '())
        (conflicts '()))
    (multiple-value-bind (ost tst file-conflicts)
        (%partition e os ts :conflict-p (lambda (o x) (not (%file-merge-pair-p o x))))
      ;; One DIFF-CONFLICT per overlapping pair of leaves.
      (dolist (c file-conflicts)
        (dolist (lo (%leaves (list (conflict-ours c))))
          (dolist (lx (%leaves (list (conflict-theirs c))))
            (push (%conflict lo lx) conflicts))))
      ;; Files both sides modified (no other conflict): merged per file.
      (dolist (o os)
        (unless (gethash o ost)
          (let ((x (find-if (lambda (x) (and (null (gethash x tst)) (%file-merge-pair-p o x))) ts)))
            (when x
              (setf (gethash o merges) t (gethash x merges) t)
              (multiple-value-bind (changes cs) (%merge-file e o x resolve)
                (setf merged (append merged changes))
                (setf conflicts (append (reverse cs) conflicts)))))))
      (dolist (o os)
        (when (and (not (gethash o merges)) (member (gethash o ost) '(nil :twin)))
          (setf merged (append merged (%leaves (list o))))))
      (dolist (x ts)
        (when (and (not (gethash x merges))
                   (or (null (gethash x tst))
                       (and (eq resolve :theirs) (eq (gethash x tst) :conflict))))
          (setf merged (append merged (%leaves (list x))))))
      (values (make-diff (%canonical-sort merged) :engine e :level :changes)
              (nreverse conflicts)))))

(defmethod rebase-states ((e tree-diff-engine) d onto base)
  (rebase-by-merge e d onto base))
