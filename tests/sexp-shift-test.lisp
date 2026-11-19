(in-package #:diff-protocol/tests)

;;; diff-protocol#2 regression: list index shifts in sexp merges.
;;; R1 rule (brief "Sexp lists in merges"): merges transform indices. In the merged
;;; diff an :ADD at list index I whose list has K removes earlier in the merged diff
;;; (from the other side) is re-indexed to I - K. A side's tail removes on one list
;;; form one run and its tail adds another; runs are atomic: when one member is in
;;; a conflict, the rest of the run stays out of the merged diff and is reported as
;;; a conflict paired with the opposite change of the run's first conflict.
;;; Row: base ours theirs merged conflict-footprints.

(defparameter *shift-cases*
  '(("remove-vs-append" ("x" "y") ("x") ("x" "y" "z")
     ("x" "z") ())
    ("remove-two-vs-append" ("x" "y" "z") ("x") ("x" "y" "z" "w")
     ("x" "w") ())
    ("nested-remove-vs-append" (:l ("x" "y") :n 1) (:l ("x") :n 1) (:l ("x" "y" "z") :n 2)
     (:l ("x" "z") :n 2) ())
    ("remove-run-atomic" ("x" "y" "z") ("x") ("x" "y" "Z")
     ("x" "y" "z") (("sexp:/1" "sexp:/2") ("sexp:/2")))
    ("add-run-atomic" ("x") ("x" "a") ("x" "b" "c")
     ("x") (("sexp:/1") ("sexp:/1" "sexp:/2")))
    ("same-tail-removed" ("a" "b" "c") ("a") ("a")
     ("a") ())
    ("shorter-tail-removed" ("a" "b" "c") ("a") ("a" "b")
     ("a") ())))

(defun shift-rows ()
  (loop for (nil base ours theirs merged conflicts) in *shift-cases*
        collect (list (q base) (q ours) (q theirs) (q merged) (q conflicts))))

(defun shift-ids () (mapcar #'first *shift-cases*))

(deftest-parametrize sexp-merge-index-shift
    ((base ours theirs merged conflicts) :ids (shift-ids) :rows (shift-rows))
  (multiple-value-bind (diff found) (d:three-way-diff *engine* base ours theirs)
    (ok (st= merged (handler-case (d:apply-diff base diff)
                      (d:apply-conflict () :apply-conflict)))
        "the merged diff applies to base")
    (ok (equal conflicts (sorted-footprints found))))
  (multiple-value-bind (diff found) (d:three-way-diff *engine* base theirs ours)
    (ok (st= merged (handler-case (d:apply-diff base diff)
                      (d:apply-conflict () :apply-conflict)))
        "symmetric")
    (ok (equal conflicts (sorted-footprints found)))))

(deftest sexp-rebase-index-shift
  ;; D appends, ONTO removes: the rebased add is re-indexed.
  (let ((base '("x" "y")))
    (multiple-value-bind (rebased conflicts)
        (d:rebase-diff (diff-of base '("x" "y" "z")) (diff-of base '("x")) base)
      (ok (null conflicts))
      (ok (equal '((1)) (paths rebased)) "add at 2 → 1")
      (ok (equal '("sexp:/1") (d:diff-footprint rebased)) "footprints follow the new index")
      (ok (st= '("x" "z") (handler-case (d:apply-diff '("x") rebased)
                            (d:apply-conflict () :apply-conflict))))))
  ;; D removes, ONTO appends: removes apply below the added tail, unchanged.
  (let ((base '("x" "y")))
    (multiple-value-bind (rebased conflicts)
        (d:rebase-diff (diff-of base '("x")) (diff-of base '("x" "y" "z")) base)
      (ok (null conflicts))
      (ok (st= '("x" "z") (handler-case (d:apply-diff '("x" "y" "z") rebased)
                            (d:apply-conflict () :apply-conflict)))))))

(deftest sexp-merged-add-reindexed
  (let ((diff (d:three-way-diff *engine* '("x" "y") '("x") '("x" "y" "z"))))
    (ok (equal '((1) (1)) (paths diff)) "remove 1 (ours), then add at 2 - 1 = 1 (theirs)")
    (ok (equal '(:remove :add) (kinds diff)))))

(deftest use-value-unreachable-keeps-restarts
  ;; diff-protocol#2, second half: USE-VALUE onto an unreachable list path re-signals
  ;; APPLY-CONFLICT with SKIP-CHANGE and USE-VALUE still available.
  (let* ((change (d:make-change :add :path '(:l 5) :footprint '("sexp:/l/5") :after 7))
         (diff (d:make-diff (list change) :engine *engine*))
         (calls 0)
         (restarts-on-second '()))
    (multiple-value-bind (state skipped)
        (handler-bind ((d:apply-conflict
                         (lambda (c)
                           (incf calls)
                           (if (= calls 1)
                               (use-value 99)
                               (progn
                                 (setf restarts-on-second
                                       (list (and (find-restart 'd:skip-change c) t)
                                             (and (find-restart 'use-value c) t)))
                                 (d:skip-change c))))))
          (d:apply-diff '(:l (1)) diff))
      (ok (= 2 calls) "use-value failed, conflict signalled again")
      (ok (equal '(t t) restarts-on-second) "skip-change and use-value available on the second signal")
      (ok (st= '(:l (1)) state))
      (ok (equal (list change) skipped)))))
