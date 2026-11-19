(in-package #:diff-protocol/tests)

;;; Laws (brief "Laws"; plan Track R law 3–4) as property cases over the corpus.

(deftest-parametrize law-apply-diff
    ((a b level) :ids (pair-level-ids) :rows (pair-level-rows))
  (ok (st= b (d:apply-diff a (diff-of a b level)))
      "apply-diff(a, diff(a, b)) = b"))

(deftest-parametrize law-invert-diff
    ((a b level) :ids (pair-level-ids) :rows (pair-level-rows))
  (ok (st= a (d:apply-diff b (d:invert-diff (diff-of a b level))))
      "apply-diff(b, invert-diff(diff(a, b))) = a"))

(deftest-parametrize law-self-diff-empty
    ((a level) :ids (state-level-ids) :rows (state-level-rows))
  (ok (d:diff-empty-p (diff-of a a level)) "diff(a, a) is empty")
  (ok (null (d:diff-footprint (diff-of a a level)))))

(deftest-parametrize law-compose
    ((a b c) :ids (triple-ids) :rows (triple-rows))
  (ok (st= c (d:apply-diff a (d:compose-diffs (diff-of a b) (diff-of b c))))
      "compose(diff(a, b), diff(b, c)) applied to a = c"))

(deftest-parametrize law-compose-associative
    ((a b c) :ids (triple-ids) :rows (triple-rows))
  (let ((d1 (diff-of a b))
        (d2 (diff-of b c))
        (d3 (diff-of c a)))
    (ok (st= (d:apply-diff a (d:compose-diffs (d:compose-diffs d1 d2) d3))
             (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3))))
        "(d1 . d2) . d3 = d1 . (d2 . d3) on a")
    (ok (st= a (d:apply-diff a (d:compose-diffs d1 (d:compose-diffs d2 d3)))))))

(deftest compose-empties
  (ok (d:diff-empty-p (d:compose-diffs (diff-of '(:a 1) '(:a 1)) (diff-of 1 1))))
  (let ((d1 (diff-of '(:a 1) '(:a 2))))
    (ok (st= '(:a 2) (d:apply-diff '(:a 1) (d:compose-diffs d1 (diff-of 1 1)))))
    (ok (st= '(:a 2) (d:apply-diff '(:a 1) (d:compose-diffs (diff-of 1 1) d1))))))

(deftest invert-shapes
  (let* ((diff (diff-of '(:a 1 :b 2 :m 5) '(:a 9 :c 3 :n 5)))
         (inv (d:invert-diff diff)))
    ;; diff: modify :a, remove :b, move :m→:n, add :c  (a's key order, then adds)
    (ok (equal '(:modify :remove :move :add) (kinds diff)))
    (ok (equal '(:remove :move :add :modify) (kinds inv)) "kinds swapped, order reversed")
    (let ((move (second (d:diff-changes inv))))
      (ok (equal '(:m) (d:change-path move)))
      (ok (equal '(:n) (d:change-from move))))
    (let ((modify (fourth (d:diff-changes inv))))
      (ok (eql 9 (d:change-before modify)))
      (ok (eql 1 (d:change-after modify))))))

(deftest invert-keeps-laziness
  (let ((inv (d:invert-diff (diff-of *level-a* *level-b* :summary))))
    (ok (notany #'d:change-before-loaded-p (d:diff-changes inv)))
    (ok (notany #'d:change-after-loaded-p (d:diff-changes inv)))
    (ok (st= *level-a* (d:apply-diff *level-b* inv)))))
