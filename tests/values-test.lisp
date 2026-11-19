(in-package #:diff-protocol/tests)

;;; Construction and accessors of change / diff values; footprint; empty-p.

(deftest change-accessors
  (let ((c (d:make-change :modify :path '(:a :b) :footprint '("sexp:/a/b")
                                  :before 1 :after 2)))
    (ok (typep c 'd:change))
    (ok (eq :modify (d:change-kind c)))
    (ok (equal '(:a :b) (d:change-path c)))
    (ok (null (d:change-from c)))
    (ok (equal '("sexp:/a/b") (d:change-footprint c)))
    (ok (eql 1 (d:change-before c)))
    (ok (eql 2 (d:change-after c)))
    (ok (null (d:change-children c)))
    (ok (d:change-before-loaded-p c))
    (ok (d:change-after-loaded-p c))))

(deftest change-move-and-children
  (let* ((child (d:make-change :modify :path '(:cfg :x) :footprint '("sexp:/cfg/x")
                                       :before 1 :after 2))
         (parent (d:make-change :modify :path '(:cfg) :footprint '("sexp:/cfg")
                                        :before '(:x 1) :after '(:x 2) :children (list child)))
         (move (d:make-change :move :path '(:z) :from '(:b)
                                    :footprint '("sexp:/b" "sexp:/z") :before 2 :after 2)))
    (ok (equal (list child) (d:change-children parent)))
    (ok (eq :move (d:change-kind move)))
    (ok (equal '(:b) (d:change-from move)))
    (ok (equal '(:z) (d:change-path move)))))

(deftest change-add-remove-sides
  (let ((add (d:make-change :add :path '(:k) :footprint '("sexp:/k") :after 1))
        (rem (d:make-change :remove :path '(:k) :footprint '("sexp:/k") :before 1)))
    (ok (null (d:change-before add)))
    (ok (eql 1 (d:change-after add)))
    (ok (eql 1 (d:change-before rem)))
    (ok (null (d:change-after rem)))))

(deftest change-kind-is-checked
  (ok (signals (d:make-change :rename :path '(:a)) 'type-error))
  (ok (signals (d:make-change "modify" :path '(:a)) 'type-error)))

(deftest change-content-is-lazy
  (let* ((calls 0)
         (c (d:make-change :modify :path '(:a) :footprint '("sexp:/a")
                                   :before 1
                                   :after-fn (lambda () (incf calls) '(:big "value")))))
    (ok (d:change-before-loaded-p c))
    (ng (d:change-after-loaded-p c))
    (ok (= 0 calls))
    (ok (equal '(:big "value") (d:change-after c)))
    (ok (equal '(:big "value") (d:change-after c)))
    (ok (= 1 calls) "thunk forced once, then cached")
    (ok (d:change-after-loaded-p c))))

(deftest opaque-change-value
  (let ((c (d:make-opaque-change :modify :footprint '("table:widgets") :description "backfill")))
    (ok (typep c 'd:opaque-change))
    (ok (typep c 'd:change))
    (ok (eq :modify (d:change-kind c)))
    (ok (equal '("table:widgets") (d:change-footprint c)))
    (ok (equal "backfill" (d:opaque-change-description c)))
    (ok (null (d:change-before c)))
    (ok (null (d:change-after c)))
    (ng (d:change-before-loaded-p c))
    (ng (d:change-after-loaded-p c))))

(deftest diff-accessors
  (let* ((c (d:make-change :add :path '(:k) :footprint '("sexp:/k") :after 1))
         (d1 (d:make-diff (list c)))
         (d2 (d:make-diff (list c) :engine *engine* :level :content)))
    (ok (typep d1 'd:diff))
    (ok (equal (list c) (d:diff-changes d1)))
    (ok (eq :changes (d:diff-level d1)) "default level")
    (ok (null (d:diff-engine d1)))
    (ok (eq *engine* (d:diff-engine d2)))
    (ok (eq :content (d:diff-level d2)))
    (ok (signals (d:make-diff (list c) :level :hunks) 'type-error))))

(deftest diff-footprint-is-union
  (let* ((child (d:make-change :modify :path '(:a :x) :footprint '("sexp:/a/x") :before 1 :after 2))
         (diff (d:make-diff
                (list (d:make-change :modify :path '(:b) :footprint '("sexp:/b") :before 1 :after 2)
                      (d:make-change :modify :path '(:a) :footprint '("sexp:/a")
                                             :before '(:x 1) :after '(:x 2) :children (list child))
                      (d:make-change :move :path '(:b) :from '(:a)
                                           :footprint '("sexp:/a" "sexp:/b") :before 3 :after 3))
                :level :content)))
    (ok (equal '("sexp:/a" "sexp:/a/x" "sexp:/b") (d:diff-footprint diff))
        "children included, deduped, sorted STRING<")
    (ok (equal '("table:widgets")
               (d:diff-footprint
                (d:make-diff (list (d:make-opaque-change :modify :footprint '("table:widgets")))))))))

(deftest diff-empty-p-cases
  (ok (d:diff-empty-p (d:make-diff '())))
  (ng (d:diff-empty-p (d:make-diff (list (d:make-change :add :path '(:k) :after 1)))))
  (ok (null (d:diff-footprint (d:make-diff '())))))

(deftest-parametrize footprint-overlap
    ((f1 f2 expected)
     :ids ("same" "child" "parent" "sibling-prefix" "root" "path-same" "path-dir"
           "path-sibling" "other-type")
     ("sexp:/a" "sexp:/a" t)
     ("sexp:/a" "sexp:/a/b" t)
     ("sexp:/a/b" "sexp:/a" t)
     ("sexp:/a" "sexp:/ab" nil)
     ("sexp:/" "sexp:/a" t)
     ("path:src/x.lisp" "path:src/x.lisp" t)
     ("path:src" "path:src/x.lisp" t)
     ("path:src/x" "path:src/xy.lisp" nil)
     ("table:widgets" "column:widgets.color" nil))
  (ok (eq expected (and (d:footprint-overlap-p f1 f2) t))))

(deftest engine-registry
  (let ((e (d:find-diff-engine :sexp)))
    (ok (typep e 'd:sexp-diff-engine))
    (ok (typep e 'd:diff-engine))
    (ok (eq :sexp (d:engine-name e)))
    (ok (eq :sexp (d:engine-name *engine*)))
    (ok (null (d:find-diff-engine :no-such-engine)))))
