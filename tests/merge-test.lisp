(in-package #:diff-protocol/tests)

;;; three-way-diff and rebase-diff: conflicts are data, symmetric in ours/theirs.
;;; Row: base ours theirs merged conflict-footprints (sorted list of sorted lists).

(defparameter *merge-cases*
  '(("disjoint" (:a 1 :b 2 :c 3) (:a 10 :b 2 :c 3) (:a 1 :b 20 :c 3)
     (:a 10 :b 20 :c 3) ())
    ("identical" (:a 1 :b 2 :c 3) (:a 10 :b 2 :c 3) (:a 10 :b 2 :c 3)
     (:a 10 :b 2 :c 3) ())
    ("same-key" (:a 1 :b 2 :c 3) (:a 10 :b 2 :c 3) (:a 11 :b 2 :c 3)
     (:a 1 :b 2 :c 3) (("sexp:/a")))
    ("nested-disjoint" (:cfg (:x 1 :y 2)) (:cfg (:x 5 :y 2)) (:cfg (:x 1 :y 6))
     (:cfg (:x 5 :y 6)) ())
    ("prefix-overlap" (:cfg (:x 1 :y 2) :n 0) (:cfg (:x 5 :y 2) :n 0) (:n 0)
     (:cfg (:x 1 :y 2) :n 0) (("sexp:/cfg" "sexp:/cfg/x")))
    ("add-add" (:a 1) (:a 1 :b 2) (:a 1 :b 3)
     (:a 1) (("sexp:/b")))
    ("list-edit-and-append" ("a" "b") ("a" "B") ("a" "b" "c")
     ("a" "B" "c") ())
    ("move-and-modify" (:a 1 :b 2) (:z 1 :b 2) (:a 1 :b 3)
     (:z 1 :b 3) ())
    ("two-leaves-vs-remove" (:cfg (:x 1 :y 2)) (:cfg (:x 5 :y 6)) ()
     (:cfg (:x 1 :y 2)) (("sexp:/cfg" "sexp:/cfg/x") ("sexp:/cfg" "sexp:/cfg/y")))))

(defun merge-rows ()
  (loop for (nil base ours theirs merged conflicts) in *merge-cases*
        collect (list (q base) (q ours) (q theirs) (q merged) (q conflicts))))

(defun merge-ids () (mapcar #'first *merge-cases*))

(deftest-parametrize three-way
    ((base ours theirs merged conflicts) :ids (merge-ids) :rows (merge-rows))
  (multiple-value-bind (diff found) (d:three-way-diff *engine* base ours theirs)
    (ok (typep diff 'd:diff))
    (ok (every (lambda (c) (typep c 'd:diff-conflict)) found) "conflicts are data")
    (ok (st= merged (d:apply-diff base diff)))
    (ok (equal conflicts (sorted-footprints found)))))

(deftest-parametrize three-way-symmetric
    ((base ours theirs merged conflicts) :ids (merge-ids) :rows (merge-rows))
  (multiple-value-bind (d1 c1) (d:three-way-diff *engine* base ours theirs)
    (multiple-value-bind (d2 c2) (d:three-way-diff *engine* base theirs ours)
      (ok (st= (d:apply-diff base d1) (d:apply-diff base d2)) "same merged state")
      (ok (equal (sorted-footprints c1) (sorted-footprints c2)) "same conflict footprints")
      (ok (= (length c1) (length c2)))
      (ok (st= merged (d:apply-diff base d2)))
      (ok (equal conflicts (sorted-footprints c2))))))

(deftest three-way-conflict-sides
  (let ((base '(:a 1)) (ours '(:a 10)) (theirs '(:a 11)))
    (let ((c (first (nth-value 1 (d:three-way-diff *engine* base ours theirs))))
          (s (first (nth-value 1 (d:three-way-diff *engine* base theirs ours)))))
      (ok (eql 10 (d:change-after (d:conflict-ours c))))
      (ok (eql 11 (d:change-after (d:conflict-theirs c))))
      (ok (eql 11 (d:change-after (d:conflict-ours s))) "swapped call swaps sides")
      (ok (eql 10 (d:change-after (d:conflict-theirs s))))
      (ok (equal '("sexp:/a") (d:conflict-footprint c))))))

(deftest three-way-merges-leaves
  (let ((diff (d:three-way-diff *engine* '(:cfg (:x 1 :y 2)) '(:cfg (:x 5 :y 2)) '(:cfg (:x 1 :y 6)))))
    (ok (eq :changes (d:diff-level diff)))
    (ok (equal '((:cfg :x) (:cfg :y)) (paths diff)) "ours first, then theirs")))

(deftest-parametrize rebase
    ((base ours theirs merged conflicts) :ids (merge-ids) :rows (merge-rows))
  ;; Rebase THEIRS's diff onto OURS's diff, both against BASE.
  (multiple-value-bind (rebased found)
      (d:rebase-diff (diff-of base theirs) (diff-of base ours) base)
    (ok (typep rebased 'd:diff))
    (ok (every (lambda (c) (typep c 'd:diff-conflict)) found))
    (ok (equal conflicts (sorted-footprints found)))
    (ok (st= (if conflicts ours merged)
             (d:apply-diff (d:apply-diff base (diff-of base ours)) rebased))
        "rebased diff applies after ONTO")))

(deftest rebase-conflict-sides
  (let ((c (first (nth-value 1 (d:rebase-diff (diff-of '(:a 1) '(:a 11))
                                              (diff-of '(:a 1) '(:a 10))
                                              '(:a 1))))))
    (ok (eql 11 (d:change-after (d:conflict-ours c))) "ours = from D")
    (ok (eql 10 (d:change-after (d:conflict-theirs c))) "theirs = from ONTO")))
