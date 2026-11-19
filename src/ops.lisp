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
            ;; One RESTART-CASE per attempt: when a USE-VALUE cannot be placed, its
            ;; APPLY-CONFLICT is signalled with SKIP-CHANGE / USE-VALUE / ABORT again
            ;; (diff-protocol#2). The restarts cover any condition the engine signals
            ;; for this change (APPLY-CONFLICT, UNSUPPORTED-CONTENT).
            (let ((attempt (lambda () (%apply-change e current c d))))
              (loop
                (restart-case (progn (setf current (funcall attempt))
                                     (return))
                  (skip-change ()
                    :report "Leave this location as it is and continue."
                    (push c skipped)
                    (return))
                  (use-value (value)
                    :report "Put a value at this change's path and continue."
                    :interactive (lambda ()
                                   (format *query-io* "Value: ")
                                   (force-output *query-io*)
                                   (list (read *query-io*)))
                    (setf attempt (lambda () (%put-value e current c value d)))))))))
      (abort ()
        :report "Abort APPLY-DIFF and return the original state."
        (values state :aborted)))))

;;; Invert

(defun %range-footprint-p (f)
  "→ (values start slash-position) when F is a \"text:…/<s>-<e>\" range string."
  (let* ((n (length f))
         (slash (position #\/ f :from-end t))
         (dash (and slash (position #\- f :start slash))))
    (when (and (> n 5) (string= "text:" f :end2 5) slash dash
               (< (1+ slash) dash (1- n))
               (every #'digit-char-p (subseq f (1+ slash) dash))
               (every #'digit-char-p (subseq f (1+ dash))))
      (values (parse-integer f :start (1+ slash) :end dash) slash))))

(defun %inverse-footprint (f before-box)
  "A \"text:…/S-E\" range is recomputed as S-(S+|before|) from the inverse's BEFORE-BOX
unless that box is a pending thunk (never forced; an absent side has no lines); any
other string is kept."
  (multiple-value-bind (start slash) (%range-footprint-p f)
    (if (and start (null (%lazy-fn before-box)) (listp (%lazy-value before-box)))
        (format nil "~A~D-~D" (subseq f 0 (1+ slash)) start
                (+ start (length (%lazy-value before-box))))
        f)))

(defmethod %invert-change ((c change) d)
  (let ((kind (change-kind c)))
    (make-instance 'change
                   :kind (case kind (:add :remove) (:remove :add) (t kind))
                   :path (if (eq kind :move) (change-from c) (change-path c))
                   :from (if (eq kind :move) (change-path c) nil)
                   :footprint (if (eq kind :move)
                                  (reverse (change-footprint c))
                                  (mapcar (lambda (f) (%inverse-footprint f (%after-box c)))
                                          (change-footprint c)))
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

(defun %partition (e xs ys &key (conflict-p (constantly t)))
  "Pair change lists XS (ours) and YS (theirs). → (values x-status y-status conflicts twins).
A status table maps a change to :TWIN or :CONFLICT (absent = free). Identical changes
(%SAME-CHANGE-P) pair up one to one first as twins; twins never conflict (each side is
self-consistent). Every other overlapping pair for which CONFLICT-P is true is one
DIFF-CONFLICT (ours from XS); pairs where CONFLICT-P is false are left to the caller.
TWINS is an EQ table change → its partner."
  (let ((xst (make-hash-table :test #'eq))
        (yst (make-hash-table :test #'eq))
        (twins (make-hash-table :test #'eq))
        (conflicts '()))
    (dolist (x xs)
      (let ((y (find-if (lambda (y) (and (not (gethash y twins)) (%same-change-p e x y))) ys)))
        (when y
          (setf (gethash x twins) y (gethash y twins) x
                (gethash x xst) :twin (gethash y yst) :twin))))
    (dolist (x xs)
      (unless (gethash x twins)
        (dolist (y ys)
          (when (and (not (gethash y twins))
                     (%changes-overlap-p x y)
                     (funcall conflict-p x y))
            (setf (gethash x xst) :conflict (gethash y yst) :conflict)
            (push (%conflict x y) conflicts)))))
    (values xst yst (nreverse conflicts) twins)))

;;; diff-protocol#2: list tail runs and index transforms (sexp lists)

(defun %list-step-p (c)
  "True for an :ADD / :REMOVE whose last path step is a list index."
  (and (member (change-kind c) '(:add :remove))
       (change-path c)
       (integerp (car (last (change-path c))))))

(defun %tail-runs (changes)
  "Runs of CHANGES, in diff order: per list (parent path) the :REMOVEs form one run and
the :ADDs another."
  (let ((groups '()))
    (dolist (c changes)
      (when (%list-step-p c)
        (let* ((key (list (change-kind c) (butlast (change-path c))))
               (cell (assoc key groups :test #'equal)))
          (if cell
              (push c (cdr cell))
              (push (list key c) groups)))))
    (mapcar (lambda (cell) (reverse (cdr cell))) (nreverse groups))))

(defun %close-runs (xs ys xst yst twins conflicts)
  "Atomic tail runs: when one member of a run is in a conflict, every member leaves the
merged diff; members without a conflict of their own are reported paired with the
opposite change of the run's first conflicting member. A twin taken out this way takes
its partner with it. → CONFLICTS plus the new ones (ours always from XS)."
  (let ((opp (make-hash-table :test #'eq))
        (extra '()))
    (dolist (c conflicts)
      (unless (gethash (conflict-ours c) opp)
        (setf (gethash (conflict-ours c) opp) (conflict-theirs c)))
      (unless (gethash (conflict-theirs c) opp)
        (setf (gethash (conflict-theirs c) opp) (conflict-ours c))))
    (let ((runs (list (cons :x (%tail-runs xs)) (cons :y (%tail-runs ys)))))
      (loop
        (let ((changed nil))
          (dolist (side runs)
            (let ((own (if (eq (car side) :x) xst yst))
                  (other (if (eq (car side) :x) yst xst)))
              (dolist (run (cdr side))
                (let ((m0 (find-if (lambda (m) (eq (gethash m own) :conflict)) run)))
                  (when m0
                    (let ((o (gethash m0 opp)))
                      (dolist (m run)
                        (unless (eq (gethash m own) :conflict)
                          (setf changed t)
                          (let ((partner (gethash m twins)))
                            (setf (gethash m own) :conflict)
                            (when o
                              (setf (gethash m opp) o)
                              (push (if (eq (car side) :x) (%conflict m o) (%conflict o m))
                                    extra))
                            (when partner
                              (setf (gethash partner other) :conflict)
                              (unless (gethash partner opp)
                                (setf (gethash partner opp) m))))))))))))
          (unless changed (return)))))
    (append conflicts (nreverse extra))))

(defun %reindex-add (c index)
  "Copy of the list :ADD C at INDEX; footprint follows the new path (boxes shared)."
  (let ((path (append (butlast (change-path c)) (list index))))
    (make-instance 'change :kind :add :path path :footprint (list (%fp path))
                           :before-box (%before-box c) :after-box (%after-box c))))

(defun %reindex-adds (changes &optional seed)
  "An :ADD at list index I after K :REMOVEs on the same list earlier in CHANGES (plus
SEED's count for that list, an alist parent → K) is re-indexed to I - K. A side has
either removes or adds on one list, so the removes counted are the other side's."
  (let ((removed (copy-alist seed)))
    (mapcar (lambda (c)
              (if (%list-step-p c)
                  (let* ((parent (butlast (change-path c)))
                         (cell (assoc parent removed :test #'equal)))
                    (if (eq (change-kind c) :remove)
                        (progn (if cell (incf (cdr cell)) (push (cons parent 1) removed))
                               c)
                        (if (and cell (plusp (cdr cell)))
                            (%reindex-add c (- (car (last (change-path c))) (cdr cell)))
                            c)))
                  c))
            changes)))

(defun %count-removes (changes)
  "Alist list parent path → number of list :REMOVEs in CHANGES."
  (let ((out '()))
    (dolist (c changes out)
      (when (and (%list-step-p c) (eq (change-kind c) :remove))
        (let* ((parent (butlast (change-path c)))
               (cell (assoc parent out :test #'equal)))
          (if cell (incf (cdr cell)) (push (cons parent 1) out)))))))

;;; Internal merge hooks

(defgeneric %three-way (engine base ours theirs resolve)
  (:documentation "→ (values merged-diff conflicts) against BASE. RESOLVE :BASE keeps base
where the sides conflict (THREE-WAY-DIFF); :THEIRS takes THEIRS there (REBASE-DIFF)."))

(defgeneric %rebase (engine d onto base)
  (:documentation "→ (values diff-after-onto conflicts) for REBASE-DIFF."))

(defun %merge-leaves (e os ts resolve)
  "Leaf merge (sexp and the default): twins once, conflicts out (or THEIRS's with RESOLVE
:THEIRS), atomic tail runs, adds re-indexed. → (values merged-changes conflicts)."
  (multiple-value-bind (ost tst conflicts twins) (%partition e os ts)
    (setf conflicts (%close-runs os ts ost tst twins conflicts))
    (values (%reindex-adds
             (append (remove-if (lambda (o) (eq (gethash o ost) :conflict)) os)
                     (remove-if (lambda (x)
                                  (case (gethash x tst)
                                    ((nil) nil)
                                    (:conflict (not (eq resolve :theirs)))
                                    (t t)))
                                ts)))
            conflicts)))

(defmethod %three-way ((e diff-engine) base ours theirs resolve)
  (multiple-value-bind (merged conflicts)
      (%merge-leaves e
                     (diff-changes (compute-diff e base ours :level :changes))
                     (diff-changes (compute-diff e base theirs :level :changes))
                     resolve)
    (values (make-diff merged :engine e :level :changes) conflicts)))

(defmethod three-way-diff ((e diff-engine) base ours theirs)
  (%three-way e base ours theirs :base))

(defmethod %rebase ((e diff-engine) d onto base)
  (declare (ignore base))           ; leaves carry absolute paths
  (let ((ds (%leaves (diff-changes d)))
        (os (%leaves (diff-changes onto))))
    (multiple-value-bind (dst ost conflicts twins) (%partition e ds os)
      (setf conflicts (%close-runs ds os dst ost twins conflicts))
      (values (make-diff (%reindex-adds (remove-if (lambda (c) (gethash c dst)) ds)
                                        (%count-removes os))
                         :engine e :level :changes)
              conflicts))))

(defun %rebase-by-merge (e d onto base)
  "Text / tree rebase: three-way in state space, conflicts resolved to ONTO; the result is
the diff from ONTO's state to the merged state."
  (let ((ours (apply-diff base d))
        (theirs (apply-diff base onto)))
    (multiple-value-bind (resolved conflicts) (%three-way e base ours theirs :theirs)
      (values (compute-diff e theirs (apply-diff base resolved))
              conflicts))))

(defmethod rebase-diff ((d diff) (onto diff) base)
  (%rebase (%default-engine d onto) d onto base))
