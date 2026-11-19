(in-package #:diff-protocol)

;;; Engine-independent operations: apply (restarts), invert, compose,
;;; three-way merge and rebase (conflicts as data).

(defun %default-engine (&rest diffs)
  (or (some (lambda (d) (and d (diff-engine d))) diffs)
      (find-diff-engine :sexp)))

;;; Apply

(defmethod apply-diff (state (d diff))
  (let ((e (%default-engine d)))
    (restart-case
        (let ((current state)
              (skipped '()))
          (dolist (c (%leaves (diff-changes d)) (values current (nreverse skipped)))
            (restart-case (setf current (%apply-change e current c d))
              (skip-change ()
                :report "Leave this location as it is and continue."
                (push c skipped))
              (use-value (value)
                :report "Put a value at this change's path and continue."
                :interactive (lambda ()
                               (format *query-io* "Value: ")
                               (force-output *query-io*)
                               (list (read *query-io*)))
                (setf current (%put-value e current c value d))))))
      (abort ()
        :report "Abort APPLY-DIFF and return the original state."
        (values state :aborted)))))

;;; Invert

(defmethod %invert-change ((c change) d)
  (let ((kind (change-kind c)))
    (make-instance 'change
                   :kind (case kind (:add :remove) (:remove :add) (t kind))
                   :path (if (eq kind :move) (change-from c) (change-path c))
                   :from (if (eq kind :move) (change-path c) nil)
                   :footprint (if (eq kind :move)
                                  (reverse (change-footprint c))
                                  (copy-list (change-footprint c)))
                   ;; Swap the boxes: nothing is forced, a later force is shared.
                   :before-box (%after-box c)
                   :after-box (%before-box c)
                   :children (%invert-changes (change-children c) d))))

(defmethod %invert-change ((c opaque-change) d)
  (error 'irreversible-change :change c :diff d))

(defun %invert-changes (changes d)
  "Inverses of CHANGES in reverse order, with SKIP-CHANGE / USE-VALUE per change."
  (let ((out '()))
    (dolist (c changes out)
      (restart-case (push (%invert-change c d) out)
        (skip-change ()
          :report "Omit this change from the inverse."
          nil)
        (use-value (inverse)
          :report "Use a given change as the inverse."
          :interactive (lambda ()
                         (format *query-io* "Inverse change (evaluated): ")
                         (force-output *query-io*)
                         (list (eval (read *query-io*))))
          (check-type inverse change)
          (push inverse out))))))

(defmethod invert-diff ((d diff))
  (make-diff (%invert-changes (diff-changes d) d)
             :engine (diff-engine d) :level (diff-level d)))

;;; Compose

(defmethod compose-diffs ((d1 diff) (d2 diff))
  (make-diff (append (diff-changes d1) (diff-changes d2))
             :engine (or (diff-engine d1) (diff-engine d2))
             :level (if (eq (diff-level d1) (diff-level d2)) (diff-level d1) :content)))

;;; Merge / rebase

(defun %same-change-p (e x y)
  "Same kind, EQUAL path (and from), STATE-EQUAL after: the change is kept once."
  (and (not (typep x 'opaque-change))
       (not (typep y 'opaque-change))
       (eq (change-kind x) (change-kind y))
       (equal (change-path x) (change-path y))
       (equal (change-from x) (change-from y))
       (state-equal e (change-after x) (change-after y))))

(defun %changes-overlap-p (x y)
  (some (lambda (f) (some (lambda (g) (footprint-overlap-p f g)) (change-footprint y)))
        (change-footprint x)))

(defun %conflict (ours theirs)
  (make-instance 'diff-conflict
                 :ours ours :theirs theirs
                 :footprint (%sorted-footprint (list (change-footprint ours)
                                                     (change-footprint theirs)))))

(defun %partition (e xs ys)
  "Pair leaf lists XS and YS. → (values x-status y-status conflicts); a status table maps
a change to :TWIN or :CONFLICT (absent = free). :CONFLICT beats :TWIN, so the
partition is symmetric in XS / YS. One DIFF-CONFLICT per overlapping non-identical pair."
  (let ((xst (make-hash-table :test #'eq))
        (yst (make-hash-table :test #'eq))
        (conflicts '()))
    (flet ((mark (table c status)
             (unless (eq (gethash c table) :conflict)
               (setf (gethash c table) status))))
      (dolist (x xs)
        (dolist (y ys)
          (when (%changes-overlap-p x y)
            (cond ((%same-change-p e x y)
                   (mark xst x :twin) (mark yst y :twin))
                  (t
                   (mark xst x :conflict) (mark yst y :conflict)
                   (push (%conflict x y) conflicts)))))))
    (values xst yst (nreverse conflicts))))

(defmethod three-way-diff ((e diff-engine) base ours theirs)
  (let ((os (diff-changes (compute-diff e base ours :level :changes)))
        (ts (diff-changes (compute-diff e base theirs :level :changes))))
    (multiple-value-bind (ost tst conflicts) (%partition e os ts)
      (values (make-diff (append (remove-if (lambda (o) (eq (gethash o ost) :conflict)) os)
                                 (remove-if (lambda (x) (gethash x tst)) ts))
                         :engine e :level :changes)
              conflicts))))

(defmethod rebase-diff ((d diff) (onto diff) base)
  (declare (ignore base))           ; leaves carry absolute paths
  (let* ((e (%default-engine d onto))
         (ds (%leaves (diff-changes d))))
    (multiple-value-bind (dst ost conflicts) (%partition e ds (%leaves (diff-changes onto)))
      (declare (ignore ost))
      (values (make-diff (remove-if (lambda (c) (gethash c dst)) ds)
                         :engine e :level :changes)
              conflicts))))
