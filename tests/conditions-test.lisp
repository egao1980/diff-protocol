(in-package #:diff-protocol/tests)

;;; apply-conflict (restarts skip-change / use-value / abort), irreversible-change
;;; (restarts skip-change / use-value), unsupported-content.
;;; Restarting forms run outside OK (Rove handler-cases inside OK).

(defun capture (thunk type)
  "Run THUNK; return the error it signals when that is of TYPE, else NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (c) (and (typep c type) c))))

(defparameter *ab* '(:a 1 :b 2))
(defparameter *ab-new* '(:a 10 :b 20))

(deftest apply-conflict-signalled
  (let ((diff (diff-of '(:a 1) '(:a 2))))
    (ok (signals (d:apply-diff '(:a 7) diff) 'd:apply-conflict))
    (ok (signals (d:apply-diff '(:a 7) diff) 'd:diff-error))))

(deftest-parametrize apply-conflict-reason
    ((from to state reason actual)
     :ids ("before-mismatch" "missing" "exists" "missing-parent")
     ('(:a 1) '(:a 2) '(:a 7) :before-mismatch 7)
     ('(:a 1 :b 2) '(:a 1) '(:a 1) :missing nil)
     ('(:a 1) '(:a 1 :b 2) '(:a 1 :b 9) :exists 9)
     ('(:cfg (:x 1)) '(:cfg (:x 2)) '(:other 1) :missing nil))
  (let ((c (capture (lambda () (d:apply-diff state (diff-of from to))) 'd:apply-conflict)))
    (ok (typep c 'd:apply-conflict))
    (ok (and c (eq reason (d:apply-conflict-reason c))))
    (ok (and c (equal actual (d:apply-conflict-actual c))))
    (ok (typep (and c (d:diff-error-change c)) 'd:change))))

(deftest apply-conflict-restarts-offered
  (let ((names '()))
    (handler-bind ((d:apply-conflict
                     (lambda (c)
                       (setf names (loop for n in '(d:skip-change use-value abort)
                                         when (find-restart n c) collect n))
                       (abort c))))
      (d:apply-diff '(:a 7 :b 2) (diff-of *ab* *ab-new*)))
    (ok (equal '(d:skip-change use-value abort) names))))

(deftest apply-conflict-skip-change
  (multiple-value-bind (out skipped)
      (handler-bind ((d:apply-conflict (lambda (c) (d:skip-change c))))
        (d:apply-diff '(:a 7 :b 2) (diff-of *ab* *ab-new*)))
    (ok (st= '(:a 7 :b 20) out) "conflicting change left, the rest applied")
    (ok (= 1 (length skipped)))
    (ok (equal '(:a) (d:change-path (first skipped))))))

(deftest apply-conflict-invoke-restart-by-name
  (let ((out (handler-bind ((d:apply-conflict (lambda (c) (declare (ignore c))
                                                (invoke-restart 'd:skip-change))))
               (d:apply-diff '(:a 7 :b 2) (diff-of *ab* *ab-new*)))))
    (ok (st= '(:a 7 :b 20) out))))

(deftest apply-conflict-use-value
  (let ((out (handler-bind ((d:apply-conflict (lambda (c) (use-value 99 c))))
               (d:apply-diff '(:a 7 :b 2) (diff-of *ab* *ab-new*)))))
    (ok (st= '(:a 99 :b 20) out))))

(deftest apply-conflict-use-value-on-remove
  (let ((out (handler-bind ((d:apply-conflict (lambda (c) (use-value :kept c))))
               (d:apply-diff '(:a 1 :b 9) (diff-of '(:a 1 :b 2) '(:a 1))))))
    (ok (st= '(:a 1 :b :kept) out))))

(deftest apply-conflict-abort
  (let ((state (list :a 7 :b 2)))
    (multiple-value-bind (out flag)
        (handler-bind ((d:apply-conflict (lambda (c) (abort c))))
          (d:apply-diff state (diff-of *ab* *ab-new*)))
      (ok (eq state out) "original state object returned")
      (ok (eq :aborted flag))
      (ok (equal '(:a 7 :b 2) state) "not mutated"))))

(defun opaque-diff ()
  (d:make-diff (list (d:make-change :modify :path '(:a) :footprint '("sexp:/a") :before 1 :after 2)
                     (d:make-opaque-change :modify :footprint '("table:widgets")
                                                   :description "backfill"))
               :engine *engine*))

(deftest irreversible-change-signalled
  (ok (signals (d:invert-diff (opaque-diff)) 'd:irreversible-change))
  (ok (signals (d:invert-diff (opaque-diff)) 'd:diff-error))
  (let ((c (capture (lambda () (d:invert-diff (opaque-diff))) 'd:irreversible-change)))
    (ok (typep (and c (d:diff-error-change c)) 'd:opaque-change))))

(deftest irreversible-change-skip-change
  (let ((inv (handler-bind ((d:irreversible-change (lambda (c) (d:skip-change c))))
               (d:invert-diff (opaque-diff)))))
    (ok (equal '(:modify) (kinds inv)))
    (ok (eql 2 (d:change-before (first (d:diff-changes inv)))))
    (ok (eql 1 (d:change-after (first (d:diff-changes inv)))))))

(deftest irreversible-change-use-value
  (let* ((undo (d:make-opaque-change :modify :footprint '("table:widgets") :description "undo backfill"))
         (inv (handler-bind ((d:irreversible-change (lambda (c) (use-value undo c))))
                (d:invert-diff (opaque-diff)))))
    (ok (= 2 (length (d:diff-changes inv))))
    (ok (eq undo (first (d:diff-changes inv))) "order reversed: the opaque inverse comes first")))

(deftest apply-opaque-unsupported
  (ok (signals (d:apply-diff '(:a 1) (opaque-diff)) 'd:unsupported-content)))
