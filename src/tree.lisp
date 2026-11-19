(in-package #:diff-protocol)

;;; R1 tree values (brief: "Tree values"). Plain data, so the :SEXP render
;;; carries trees unchanged:
;;;   directory  (:dir (name node) ...)        entries sorted STRING< by name
;;;   file       (:file :type T :content C :mode M)
;;; Empty subdirectories are not represented. A file mode NIL reads as #o100644.
;;; CONTRACT SKELETON: stubs only.

(defun make-tree (&rest names-and-nodes)
  "→ directory node from alternating NAME NODE arguments. NAME: non-empty string
without #\\/ (else TYPE-ERROR); NODE: a directory or file node (else TYPE-ERROR);
duplicate names → ERROR. Entries are sorted STRING<; empty subdirectories are dropped.
(make-tree) is the empty tree."
  (declare (ignore names-and-nodes))
  (%r1-todo 'make-tree))

(defun make-tree-file (content &key type mode)
  "→ file node. TYPE defaults to :TEXT for a string, :BINARY for an
(unsigned-byte 8) vector, :SEXP otherwise. MODE: NIL or an integer (#o100644,
#o100755, #o120000)."
  (declare (ignore content type mode))
  (%r1-todo 'make-tree-file))

(defun tree-dir-p (x)
  "True when X is a directory node."
  (declare (ignore x))
  (%r1-todo 'tree-dir-p))

(defun tree-file-p (x)
  "True when X is a file node."
  (declare (ignore x))
  (%r1-todo 'tree-file-p))

(defun tree-entries (dir)
  "→ fresh list of (name node), sorted STRING< by name."
  (declare (ignore dir))
  (%r1-todo 'tree-entries))

(defun tree-file-type (file)
  (declare (ignore file))
  (%r1-todo 'tree-file-type))

(defun tree-file-content (file)
  (declare (ignore file))
  (%r1-todo 'tree-file-content))

(defun tree-file-mode (file)
  "The mode as given (NIL when unspecified)."
  (declare (ignore file))
  (%r1-todo 'tree-file-mode))

(defun tree-get (tree path)
  "→ (values node found-p) at PATH, a list of names. NIL → TREE itself."
  (declare (ignore tree path))
  (%r1-todo 'tree-get))
