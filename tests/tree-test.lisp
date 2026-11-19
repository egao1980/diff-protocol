(in-package #:diff-protocol/tests)

;;; R1 tree values: plain data built by MAKE-TREE / MAKE-TREE-FILE.

(deftest tree-file-values
  (let ((f (d:make-tree-file (nl "x"))))
    (ok (d:tree-file-p f))
    (ok (not (d:tree-dir-p f)))
    (ok (eq :text (d:tree-file-type f)) "string → :text")
    (ok (equal (nl "x") (d:tree-file-content f)))
    (ok (null (d:tree-file-mode f))))
  (ok (eq :binary (d:tree-file-type (d:make-tree-file (octets 1 2)))) "octets → :binary")
  (ok (eq :sexp (d:tree-file-type (d:make-tree-file '(:a 1)))) "other → :sexp")
  (ok (eq :sexp (d:tree-file-type (d:make-tree-file "s" :type :sexp))) "explicit type wins")
  (ok (eql #o100755 (d:tree-file-mode (d:make-tree-file "" :mode #o100755)))))

(deftest tree-dir-values
  (let* ((f (d:make-tree-file (nl "x")))
         (tree (d:make-tree "b" f "a" (d:make-tree "c" f) "e" (d:make-tree))))
    (ok (d:tree-dir-p tree))
    (ok (not (d:tree-file-p tree)))
    (ok (equal '("a" "b") (mapcar #'first (d:tree-entries tree)))
        "sorted by name; the empty subdirectory is dropped")
    (ok (d:tree-dir-p (second (first (d:tree-entries tree)))))
    (ok (equal (nl "x") (d:tree-file-content (d:tree-get tree '("a" "c")))))
    (ok (eq tree (d:tree-get tree '())) "root")
    (multiple-value-bind (node found) (d:tree-get tree '("a" "zz"))
      (ok (null node))
      (ok (not found)))
    (multiple-value-bind (node found) (d:tree-get tree '("b" "c"))
      (ok (null node) "a file has no children")
      (ok (not found)))
    (ok (null (d:tree-entries (d:make-tree))) "empty tree")
    (ok (d:tree-dir-p (d:make-tree)))))

(deftest tree-values-are-plain-data
  (let ((tree (d:make-tree "a" (d:make-tree-file (nl "x") :mode #o100755))))
    (ok (equal tree (read-from-string (prin1-to-string tree))) "prints and reads back EQUAL")
    (ok (equal tree (d:make-tree "a" (d:make-tree-file (nl "x") :mode #o100755)))
        "construction is deterministic")))

(deftest tree-constructor-errors
  (let ((f (d:make-tree-file "")))
    (ok (d:tree-dir-p (d:make-tree "ok" f)))
    (ok (signals (d:make-tree "a/b" f) 'type-error) "a name has no slash")
    (ok (signals (d:make-tree "" f) 'type-error) "a name is not empty")
    (ok (signals (d:make-tree :a f) 'type-error) "a name is a string")
    (ok (signals (d:make-tree "a" 42) 'type-error) "a node is a tree or a file")
    (ok (signals (d:make-tree "a") 'error) "names and nodes alternate")
    (ok (signals (d:make-tree "a" f "a" f) 'error) "duplicate names")))

(deftest tree-state-equal
  (ok (tr= (tr `(("a" ,(nl "x")) ("b/c" "y"))) (tr `(("b/c" "y") ("a" ,(nl "x"))))))
  (ok (tr= (tr `(("a" ,(nl "x")))) (tr `(("a" ,(nl "x") :mode #o100644))))
      "mode NIL reads as #o100644")
  (ok (not (tr= (tr `(("a" ,(nl "x")))) (tr `(("a" ,(nl "x") :mode #o100755))))))
  (ok (not (tr= (tr `(("a" ,(nl "x")))) (tr `(("a" "x"))))))
  (ok (not (tr= (tr `(("a" "x"))) (tr `(("a" "x" :type :sexp))))) "type matters")
  (ok (tr= (tr '(("c" (:a 1 :b 2) :type :sexp))) (tr '(("c" (:b 2 :a 1) :type :sexp))))
      "leaf content compared by the leaf engine")
  (ok (tr= (tr `(("l" ,(octets 1 2)))) (tr `(("l" ,(octets 1 2))))))
  (ok (not (tr= (tr '()) (tr '(("a" ""))))))
  (ok (tr= (d:make-tree) (tr '()))))
