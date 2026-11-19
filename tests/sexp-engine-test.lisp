(in-package #:diff-protocol/tests)

;;; The sexp / plist engine: compute-diff shapes, levels, footprints, apply.

(deftest-parametrize state-equal-cases
    ((a b expected)
     :ids ("plist-order" "list-order" "string" "string-case" "nested-plist-order"
           "nil" "number-type" "plist-vs-list" "extra-key")
     ('(:a 1 :b 2) '(:b 2 :a 1) t)
     ('(1 2) '(2 1) nil)
     ("a" "a" t)
     ("a" "A" nil)
     ('(:a (:x 1 :y 2)) '(:a (:y 2 :x 1)) t)
     (nil nil t)
     (1 1.0 nil)
     ('(:a 1) '(:a 1 :b 2) nil)
     ('(:a 1 :b 2) '(:a 1) nil))
  (ok (eq expected (and (st= a b) t))))

(deftest-parametrize unsupported-content
    ((a b)
     :ids ("hash-table" "vector" "struct-like")
     ((make-hash-table) '(:a 1))
     ('(:a 1) (vector 1 2))
     ((make-condition 'simple-error) 1))
  (ok (signals (diff-of a b) 'd:unsupported-content))
  (ok (signals (diff-of a b) 'd:diff-error)))

(deftest flat-modify
  (let ((diff (diff-of '(:a 1 :b 2) '(:a 1 :b 3))))
    (ok (typep diff 'd:diff))
    (ok (eq :changes (d:diff-level diff)) "default level :changes")
    (ok (typep (d:diff-engine diff) 'd:sexp-diff-engine))
    (ok (= 1 (length (d:diff-changes diff))))
    (let ((c (first (d:diff-changes diff))))
      (ok (eq :modify (d:change-kind c)))
      (ok (equal '(:b) (d:change-path c)))
      (ok (equal '("sexp:/b") (d:change-footprint c)))
      (ok (eql 2 (d:change-before c)))
      (ok (eql 3 (d:change-after c))))))

(deftest flat-remove-then-add
  (let ((diff (diff-of '(:a 1 :b 2) '(:a 1 :c 3))))
    (ok (equal '(:remove :add) (kinds diff)))
    (ok (equal '((:b) (:c)) (paths diff)))
    (ok (eql 2 (d:change-before (first (d:diff-changes diff)))))
    (ok (eql 3 (d:change-after (second (d:diff-changes diff)))))))

(deftest flat-move
  (let* ((diff (diff-of '(:a 1 :b 2) '(:a 1 :z 2)))
         (c (first (d:diff-changes diff))))
    (ok (equal '(:move) (kinds diff)))
    (ok (equal '(:z) (d:change-path c)))
    (ok (equal '(:b) (d:change-from c)))
    (ok (equal '("sexp:/b" "sexp:/z") (d:change-footprint c)))
    (ok (eql 2 (d:change-after c)))))

(deftest move-pairing-order
  ;; Removed :b 7 and :c 7; added :y 7 and :z 7 → (:b → :y), (:c → :z).
  (let ((diff (diff-of '(:a 1 :b 7 :c 7) '(:a 1 :y 7 :z 7))))
    (ok (equal '(:move :move) (kinds diff)))
    (ok (equal '((:b) (:c)) (mapcar #'d:change-from (d:diff-changes diff))))
    (ok (equal '((:y) (:z)) (paths diff)))))

(deftest reordered-plist-is-empty
  (ok (d:diff-empty-p (diff-of '(:a 1 :b 2) '(:b 2 :a 1)))))

(deftest list-shrink
  (let ((diff (diff-of '(1 2 3 4) '(1 9))))
    (ok (equal '(:modify :remove :remove) (kinds diff)))
    (ok (equal '((1) (3) (2)) (paths diff)) "tail removes highest index first")
    (ok (equal '("sexp:/1" "sexp:/2" "sexp:/3") (d:diff-footprint diff)))))

(deftest list-grow
  (let ((diff (diff-of '(1) '(1 2 3))))
    (ok (equal '(:add :add) (kinds diff)))
    (ok (equal '((1) (2)) (paths diff)) "tail adds lowest index first")))

(deftest-parametrize root-modify
    ((a b)
     :ids ("atoms" "atom-vs-plist" "plist-vs-list" "list-vs-string")
     (1 "x")
     (42 '(:a 1))
     ('(:a 1) '(1 2))
     ('(1 2 3) "123"))
  (let ((diff (diff-of a b)))
    (ok (equal '(:modify) (kinds diff)))
    (ok (equal '(nil) (paths diff)))
    (ok (equal '("sexp:/") (d:diff-footprint diff)))))

(deftest nil-is-empty-plist-and-list
  (let ((d1 (diff-of nil '(:a 1)))
        (d2 (diff-of nil '(1 2)))
        (d3 (diff-of '(:x nil) '(:x (:k 1)))))
    (ok (equal '(:add) (kinds d1)))
    (ok (equal '((:a)) (paths d1)))
    (ok (equal '(:add :add) (kinds d2)))
    (ok (equal '((0) (1)) (paths d2)))
    (ok (equal '((:x :k)) (paths d3)))))

(deftest footprint-strings
  (ok (equal '("sexp:/foo-bar")
             (d:diff-footprint (diff-of '(:foo-bar 1) '(:foo-bar 2))))
      "keyword step is the lowercased symbol name")
  (ok (equal '("sexp:/items/1")
             (d:diff-footprint (diff-of '(:items (1 2)) '(:items (1 5)))))))

(defparameter *level-a* '(:name "w" :cfg (:x 1 :y (:p 1 :q 2))))
(defparameter *level-b* '(:name "v" :cfg (:x 1 :y (:p 1 :q 3)) :new 1))

(deftest level-summary
  (let ((diff (diff-of *level-a* *level-b* :summary)))
    (ok (eq :summary (d:diff-level diff)))
    (ok (equal '(:modify :modify :add) (kinds diff)))
    (ok (equal '((:name) (:cfg) (:new)) (paths diff)))
    (ok (every #'null (mapcar #'d:change-children (d:diff-changes diff))))
    (ok (notany #'d:change-after-loaded-p (d:diff-changes diff)) "content not forced")
    (ok (notany #'d:change-before-loaded-p (d:diff-changes diff)))
    (ok (equal '("sexp:/cfg" "sexp:/name" "sexp:/new") (d:diff-footprint diff)))
    (let ((cfg (second (d:diff-changes diff))))
      (ok (st= '(:x 1 :y (:p 1 :q 3)) (d:change-after cfg)))
      (ok (d:change-after-loaded-p cfg)))))

(deftest level-changes
  (let ((diff (diff-of *level-a* *level-b* :changes)))
    (ok (eq :changes (d:diff-level diff)))
    (ok (equal '(:modify :modify :add) (kinds diff)))
    (ok (equal '((:name) (:cfg :y :q) (:new)) (paths diff)) "leaves, depth-first")
    (ok (every #'d:change-after-loaded-p (d:diff-changes diff)))
    (ok (eql 3 (d:change-after (second (d:diff-changes diff)))))
    (ok (equal '("sexp:/cfg/y/q" "sexp:/name" "sexp:/new") (d:diff-footprint diff)))))

(deftest level-content
  (let* ((diff (diff-of *level-a* *level-b* :content))
         (cfg (second (d:diff-changes diff)))
         (y (first (d:change-children cfg)))
         (q (first (d:change-children y))))
    (ok (eq :content (d:diff-level diff)))
    (ok (equal '((:name) (:cfg) (:new)) (paths diff)) "roots")
    (ok (= 1 (length (d:change-children cfg))))
    (ok (equal '(:cfg :y) (d:change-path y)) "absolute child paths")
    (ok (equal '(:cfg :y :q) (d:change-path q)))
    (ok (eql 2 (d:change-before q)))
    (ok (eql 3 (d:change-after q)))
    (ok (null (d:change-children q)))
    (ok (null (d:change-children (first (d:diff-changes diff)))) "atom modify has no children")
    (ok (equal '("sexp:/cfg" "sexp:/cfg/y" "sexp:/cfg/y/q" "sexp:/name" "sexp:/new")
               (d:diff-footprint diff)))))

(deftest level-is-checked
  (ok (signals (diff-of '(:a 1) '(:a 2) :hunks) 'type-error)))

(deftest pure-inputs
  (let* ((a (copy-tree *level-a*))
         (b (copy-tree *level-b*))
         (diff (diff-of a b :content))
         (out (d:apply-diff a diff)))
    (ok (equal a *level-a*) "compute-diff / apply-diff do not mutate A")
    (ok (equal b *level-b*) "compute-diff does not mutate B")
    (ok (st= out *level-b*))))

(deftest apply-key-order
  (ok (equal '(:a 1 :b 2) (d:apply-diff '(:a 1) (diff-of '(:a 1) '(:a 1 :b 2))))
      "plist add appends the key")
  (ok (equal '(:b 2 :a 5) (d:apply-diff '(:b 2 :a 1) (diff-of '(:a 1 :b 2) '(:a 5 :b 2))))
      "existing key order kept"))

(deftest apply-returns-no-skips
  (multiple-value-bind (out skipped) (d:apply-diff '(:a 1) (diff-of '(:a 1) '(:a 2)))
    (ok (st= '(:a 2) out))
    (ok (null skipped))))
