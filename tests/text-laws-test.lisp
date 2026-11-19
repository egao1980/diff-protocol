(in-package #:diff-protocol/tests)

;;; Plan Track R laws 3-4 for the text engine (both algorithms) over the fixed
;;; text corpus (every ordered pair) and deterministic random texts.

(deftest-parametrize text-law-apply-diff
    ((engine a b level) :ids (text-pair-ids) :rows (text-pair-rows))
  (ok (tx= b (d:apply-diff a (d:compute-diff engine a b :level level)))
      "apply-diff(a, diff(a, b)) = b"))

(deftest-parametrize text-law-invert-diff
    ((engine a b level) :ids (text-pair-ids) :rows (text-pair-rows))
  (ok (tx= a (d:apply-diff b (d:invert-diff (d:compute-diff engine a b :level level))))
      "apply-diff(b, invert-diff(diff(a, b))) = a"))

(deftest-parametrize text-law-self-diff-empty
    ((engine a b level) :ids (text-pair-ids :levels '(:summary :changes :content))
                        :rows (text-pair-rows :levels '(:summary :changes :content)))
  (ok (d:diff-empty-p (d:compute-diff engine a a :level level)) "diff(a, a) is empty")
  (ok (d:diff-empty-p (d:compute-diff engine b b :level level))))

(deftest-parametrize text-law-compose
    ((engine a b c) :ids (text-triple-ids) :rows (text-triple-rows))
  (let ((d1 (d:compute-diff engine a b))
        (d2 (d:compute-diff engine b c))
        (d3 (d:compute-diff engine c a)))
    (ok (tx= c (d:apply-diff a (d:compose-diffs d1 d2))) "compose(diff(a, b), diff(b, c)) on a = c")
    (ok (tx= (d:apply-diff a (d:compose-diffs (d:compose-diffs d1 d2) d3))
             (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3))))
        "associative")
    (ok (tx= a (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3)))))))

(deftest-parametrize text-law-three-way-symmetric
    ((engine base ours theirs) :ids (text-triple-ids) :rows (text-triple-rows))
  (multiple-value-bind (d1 c1) (d:three-way-diff engine base ours theirs)
    (multiple-value-bind (d2 c2) (d:three-way-diff engine base theirs ours)
      (ok (every (lambda (c) (typep c 'd:diff-conflict)) c1) "conflicts are data")
      (ok (tx= (d:apply-diff base d1) (d:apply-diff base d2)) "same merged text")
      (ok (equal (sorted-footprints c1) (sorted-footprints c2)) "same conflict footprints")
      (ok (= (length c1) (length c2))))))

(deftest-parametrize text-law-three-way-trivial
    ((engine base ours theirs) :ids (text-triple-ids) :rows (text-triple-rows))
  (declare (ignorable theirs))
  (multiple-value-bind (d c) (d:three-way-diff engine base ours base)
    (ok (null c) "only one side changed: no conflicts")
    (ok (tx= ours (d:apply-diff base d))))
  (multiple-value-bind (d c) (d:three-way-diff engine base ours ours)
    (ok (null c) "both sides made the same change: no conflicts")
    (ok (tx= ours (d:apply-diff base d)))))
