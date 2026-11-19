(in-package #:diff-protocol/tests)

;;; Plan Track R laws 3-4 for the tree engine over the tree corpus and random
;;; trees, then tree three-way merge cases (leaf merges dispatched by type).

(deftest-parametrize tree-law-apply-diff
    ((a b level) :ids (tree-pair-ids) :rows (tree-pair-rows))
  (ok (tr= b (d:apply-diff a (d:compute-diff *tree* a b :level level)))
      "apply-diff(a, diff(a, b)) = b"))

(deftest-parametrize tree-law-invert-diff
    ((a b level) :ids (tree-pair-ids) :rows (tree-pair-rows))
  (ok (tr= a (d:apply-diff b (d:invert-diff (d:compute-diff *tree* a b :level level))))
      "apply-diff(b, invert-diff(diff(a, b))) = a"))

(deftest-parametrize tree-law-self-diff-empty
    ((a b level) :ids (tree-pair-ids) :rows (tree-pair-rows))
  (ok (d:diff-empty-p (d:compute-diff *tree* a a :level level)))
  (ok (d:diff-empty-p (d:compute-diff *tree* b b :level level))))

(deftest-parametrize tree-law-compose
    ((a b c) :ids (tree-triple-ids) :rows (tree-triple-rows))
  (let ((d1 (d:compute-diff *tree* a b))
        (d2 (d:compute-diff *tree* b c))
        (d3 (d:compute-diff *tree* c a)))
    (ok (tr= c (d:apply-diff a (d:compose-diffs d1 d2))))
    (ok (tr= (d:apply-diff a (d:compose-diffs (d:compose-diffs d1 d2) d3))
             (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3)))))
    (ok (tr= a (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3)))))))

(deftest-parametrize tree-law-three-way-symmetric
    ((base ours theirs) :ids (tree-triple-ids) :rows (tree-triple-rows))
  (multiple-value-bind (d1 c1) (d:three-way-diff *tree* base ours theirs)
    (multiple-value-bind (d2 c2) (d:three-way-diff *tree* base theirs ours)
      (ok (every (lambda (c) (typep c 'd:diff-conflict)) c1))
      (ok (tr= (d:apply-diff base d1) (d:apply-diff base d2)) "same merged tree")
      (ok (equal (sorted-footprints c1) (sorted-footprints c2)))
      (ok (= (length c1) (length c2))))))

(deftest-parametrize tree-law-three-way-trivial
    ((base ours theirs) :ids (tree-triple-ids) :rows (tree-triple-rows))
  (declare (ignorable theirs))
  (multiple-value-bind (d c) (d:three-way-diff *tree* base ours base)
    (ok (null c))
    (ok (tr= ours (d:apply-diff base d))))
  (multiple-value-bind (d c) (d:three-way-diff *tree* base ours ours)
    (ok (null c))
    (ok (tr= ours (d:apply-diff base d)))))

;;; Tree three-way cases. Row: base ours theirs merged conflict-footprints.

(defparameter *tree-merge-cases*
  `(("disjoint-files" (("a" ,(nl "x")) ("b" ,(nl "y"))) (("a" ,(nl "X")) ("b" ,(nl "y"))) (("a" ,(nl "x")) ("b" ,(nl "Y")))
     (("a" ,(nl "X")) ("b" ,(nl "Y"))) ())
    ("same-file-disjoint-hunks" (("f" ,(nl "1" "2" "3" "4" "5"))) (("f" ,(nl "A" "2" "3" "4" "5")))
     (("f" ,(nl "1" "2" "3" "4" "E"))) (("f" ,(nl "A" "2" "3" "4" "E"))) ())
    ("same-file-same-line" (("f" ,(nl "a" "b" "c"))) (("f" ,(nl "a" "B" "c"))) (("f" ,(nl "a" "X" "c")))
     (("f" ,(nl "a" "b" "c"))) (("text:/f/1-2" "tree:/f")))
    ("modify-delete" (("f" ,(nl "a" "b" "c")) ("k" "k")) (("f" ,(nl "a" "B" "c")) ("k" "k")) (("k" "k"))
     (("f" ,(nl "a" "b" "c")) ("k" "k")) (("text:/f/1-2" "tree:/f")))
    ("add-add-same" (("k" "k")) (("k" "k") ("n" ,(nl "z"))) (("k" "k") ("n" ,(nl "z")))
     (("k" "k") ("n" ,(nl "z"))) ())
    ("add-add-different" (("k" "k")) (("k" "k") ("n" ,(nl "z"))) (("k" "k") ("n" ,(nl "w")))
     (("k" "k")) (("tree:/n")))
    ("rename-and-other-edit" (("a" ,(nl "1" "2" "3" "4")) ("c" ,(nl "c"))) (("b" ,(nl "1" "2" "3" "4")) ("c" ,(nl "c")))
     (("a" ,(nl "1" "2" "3" "4")) ("c" ,(nl "C"))) (("b" ,(nl "1" "2" "3" "4")) ("c" ,(nl "C"))) ())
    ("rename-vs-modify-source" (("a" ,(nl "1" "2" "3" "4"))) (("b" ,(nl "1" "2" "3" "4")))
     (("a" ,(nl "X" "2" "3" "4"))) (("a" ,(nl "1" "2" "3" "4"))) (("text:/a/0-1" "tree:/a" "tree:/b")))
    ("file-vs-dir" (("k" "k")) (("k" "k") ("x" ,(nl "file"))) (("k" "k") ("x/y" ,(nl "nested")))
     (("k" "k")) (("tree:/x" "tree:/x/y")))
    ("mode-vs-content" (("f" ,(nl "a" "b"))) (("f" ,(nl "a" "b") :mode #o100755)) (("f" ,(nl "a" "B")))
     (("f" ,(nl "a" "B") :mode #o100755)) ())
    ("both-remove" (("f" ,(nl "a")) ("k" "k")) (("k" "k")) (("k" "k")) (("k" "k")) ())
    ("sexp-leaf-merge" (("cfg" (:a 1 :b 2) :type :sexp)) (("cfg" (:a 10 :b 2) :type :sexp))
     (("cfg" (:a 1 :b 20) :type :sexp)) (("cfg" (:a 10 :b 20) :type :sexp)) ())
    ("sexp-leaf-conflict" (("cfg" (:a 1 :b 2) :type :sexp)) (("cfg" (:a 10 :b 2) :type :sexp))
     (("cfg" (:a 11 :b 2) :type :sexp)) (("cfg" (:a 1 :b 2) :type :sexp)) (("sexp:/cfg/a" "tree:/cfg")))
    ("binary-both-changed" (("l" ,(octets 1))) (("l" ,(octets 2))) (("l" ,(octets 3)))
     (("l" ,(octets 1))) (("tree:/l")))))

(defun tree-merge-rows ()
  (loop for (nil base ours theirs merged conflicts) in *tree-merge-cases*
        collect (list (tree-form base) (tree-form ours) (tree-form theirs) (tree-form merged) (q conflicts))))

(defun tree-merge-ids () (mapcar #'first *tree-merge-cases*))

(deftest-parametrize tree-three-way
    ((base ours theirs merged conflicts) :ids (tree-merge-ids) :rows (tree-merge-rows))
  (multiple-value-bind (diff found) (d:three-way-diff *tree* base ours theirs)
    (ok (typep diff 'd:diff))
    (ok (eq :changes (d:diff-level diff)))
    (ok (every (lambda (c) (typep c 'd:diff-conflict)) found))
    (ok (tr= merged (d:apply-diff base diff)) "conflicting paths keep base")
    (ok (equal conflicts (sorted-footprints found))))
  (multiple-value-bind (diff found) (d:three-way-diff *tree* base theirs ours)
    (ok (tr= merged (d:apply-diff base diff)) "symmetric")
    (ok (equal conflicts (sorted-footprints found)))))

(deftest tree-three-way-leaf-conflict-sides
  (let ((c (first (nth-value 1 (d:three-way-diff *tree* (tr `(("f" ,(nl "a" "b" "c"))))
                                                 (tr `(("f" ,(nl "a" "B" "c"))))
                                                 (tr `(("f" ,(nl "a" "X" "c")))))))))
    (ok (typep c 'd:diff-conflict))
    (ok (equal '("f" 1) (d:change-path (d:conflict-ours c))) "nested leaf change")
    (ok (equal (list (nl "B")) (d:change-after (d:conflict-ours c))))
    (ok (equal (list (nl "X")) (d:change-after (d:conflict-theirs c))))
    (ok (equal '("tree:/f" "text:/f/1-2") (d:change-footprint (d:conflict-ours c))))))

(deftest tree-rebase
  (let* ((base (tr `(("a" ,(nl "x")) ("b" ,(nl "y")))))
         (onto-state (tr `(("a" ,(nl "X")) ("b" ,(nl "y")))))
         (d-state (tr `(("a" ,(nl "x")) ("b" ,(nl "Y")) ("c" ,(nl "new"))))))
    (multiple-value-bind (rebased conflicts)
        (d:rebase-diff (d:compute-diff *tree* base d-state) (d:compute-diff *tree* base onto-state) base)
      (ok (null conflicts))
      (ok (tr= (tr `(("a" ,(nl "X")) ("b" ,(nl "Y")) ("c" ,(nl "new"))))
               (d:apply-diff onto-state rebased))))))
