(in-package #:diff-protocol)

;;; R1 tree values (brief: "Tree values"). Plain data, so the :SEXP render
;;; carries trees unchanged:
;;;   directory  (:dir (name node) ...)        entries sorted STRING< by name
;;;   file       (:file :type T :content C :mode M)
;;; Empty subdirectories are not represented. A file mode NIL reads as #o100644.

(defun %tree-name-p (x)
  (and (stringp x) (plusp (length x)) (not (find #\/ x))))

(deftype %tree-name () '(satisfies %tree-name-p))

(defun tree-dir-p (x)
  "True when X is a directory node."
  (and (consp x) (eq (car x) :dir) (%proper-list-p x) t))

(defun tree-file-p (x)
  "True when X is a file node."
  (and (consp x) (eq (car x) :file) (%proper-list-p x) (= 7 (length x))
       (eq (nth 1 x) :type) (eq (nth 3 x) :content) (eq (nth 5 x) :mode)))

(deftype %tree-node () '(or (satisfies tree-dir-p) (satisfies tree-file-p)))

(defun %dir-has-files-p (dir)
  (some (lambda (entry)
          (let ((node (second entry)))
            (or (tree-file-p node) (and (tree-dir-p node) (%dir-has-files-p node)))))
        (rest dir)))

(defun %make-dir (entries)
  "Directory node from (name node) ENTRIES: sorted, empty subdirectories dropped."
  (cons :dir (sort (remove-if (lambda (entry)
                                (let ((node (second entry)))
                                  (and (tree-dir-p node) (not (%dir-has-files-p node)))))
                              (copy-list entries))
                   #'string< :key #'first)))

(defun make-tree (&rest names-and-nodes)
  "→ directory node from alternating NAME NODE arguments. NAME: non-empty string
without #\\/ (else TYPE-ERROR); NODE: a directory or file node (else TYPE-ERROR);
duplicate names → ERROR. Entries are sorted STRING<; empty subdirectories are dropped.
(make-tree) is the empty tree."
  (unless (evenp (length names-and-nodes))
    (error "MAKE-TREE: names and nodes must alternate, got ~S" names-and-nodes))
  (let ((entries '()))
    (loop for (name node) on names-and-nodes by #'cddr
          do (unless (%tree-name-p name)
               (error 'type-error :datum name :expected-type '%tree-name))
             (unless (typep node '%tree-node)
               (error 'type-error :datum node :expected-type '%tree-node))
             (when (assoc name entries :test #'string=)
               (error "MAKE-TREE: duplicate name ~S" name))
             (push (list name node) entries))
    (%make-dir entries)))

(defun make-tree-file (content &key type mode)
  "→ file node. TYPE defaults to :TEXT for a string, :BINARY for an
(unsigned-byte 8) vector, :SEXP otherwise. MODE: NIL or an integer (#o100644,
#o100755, #o120000)."
  (check-type type (or null keyword))
  (check-type mode (or null (integer 0)))
  (list :file
        :type (or type
                  (typecase content
                    (string :text)
                    ((vector (unsigned-byte 8)) :binary)
                    (t :sexp)))
        :content content
        :mode mode))

(defun tree-entries (dir)
  "→ fresh list of (name node), sorted STRING< by name."
  (check-type dir (satisfies tree-dir-p))
  (copy-list (rest dir)))

(defun tree-file-type (file)
  (check-type file (satisfies tree-file-p))
  (getf (rest file) :type))

(defun tree-file-content (file)
  (check-type file (satisfies tree-file-p))
  (getf (rest file) :content))

(defun tree-file-mode (file)
  "The mode as given (NIL when unspecified)."
  (check-type file (satisfies tree-file-p))
  (getf (rest file) :mode))

(defun %file-mode (file)
  "The mode with NIL read as #o100644."
  (or (tree-file-mode file) #o100644))

(defun %file-with (file &key (content nil content-p) (mode nil mode-p))
  (list :file :type (tree-file-type file)
              :content (if content-p content (tree-file-content file))
              :mode (if mode-p mode (tree-file-mode file))))

(defun tree-get (tree path)
  "→ (values node found-p) at PATH, a list of names. NIL → TREE itself."
  (cond ((null path) (values tree t))
        ((tree-dir-p tree)
         (let ((entry (assoc (first path) (rest tree) :test #'equal)))
           (if entry
               (tree-get (second entry) (rest path))
               (values nil nil))))
        (t (values nil nil))))

(defun %tree-files (dir &optional prefix)
  "→ ((names . file-node) ...) of every file under DIR, names relative to the root."
  (loop for (name node) in (rest dir)
        for names = (append prefix (list name))
        nconc (if (tree-file-p node)
                  (list (cons names node))
                  (%tree-files node names))))

(defun %tree-put (dir names node fail)
  "→ new directory with NODE at NAMES; missing directories are created. A file in the
way of a parent calls FAIL with that file."
  (let* ((name (first names))
         (entry (assoc name (rest dir) :test #'equal))
         (others (remove entry (rest dir))))
    (%make-dir
     (cons (list name
                 (if (rest names)
                     (let ((child (second entry)))
                       (cond ((null entry) (%tree-put '(:dir) (rest names) node fail))
                             ((tree-file-p child) (funcall fail child))
                             (t (%tree-put child (rest names) node fail))))
                     node))
           others))))

(defun %tree-delete (dir names)
  "→ new directory without the node at NAMES (which must be present); directories left
empty are pruned."
  (let* ((name (first names))
         (entry (assoc name (rest dir) :test #'equal))
         (others (remove entry (rest dir))))
    (%make-dir (if (rest names)
                   (cons (list name (%tree-delete (second entry) (rest names))) others)
                   others))))
