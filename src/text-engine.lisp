(in-package #:diff-protocol)

;;; R1 text engine (brief: "R1 — locked", "Text engine", "diff3").
;;; One line core shared by hunks, context, diff3, rename similarity and the
;;; unified body: Myers / histogram → ascending matched pairs → changed runs.

(defclass text-diff-engine (diff-engine)
  ((algorithm :initarg :algorithm :initform :myers :reader text-engine-algorithm))
  (:documentation "Line-based engine for strings. ENGINE-NAME → :TEXT.
ALGORITHM is :MYERS (minimal, default) or :HISTOGRAM; anything else → TYPE-ERROR
from MAKE-INSTANCE."))

(defmethod initialize-instance :after ((e text-diff-engine) &key)
  (let ((algorithm (text-engine-algorithm e)))
    (unless (member algorithm '(:myers :histogram))
      (error 'type-error :datum algorithm :expected-type '(member :myers :histogram)))))

(defmethod engine-name ((e text-diff-engine))
  :text)

(defun %register-engine (name engine)
  "Register ENGINE under NAME once (a reload keeps the shared instance)."
  (unless (find-diff-engine name)
    (setf *diff-engines* (append *diff-engines* (list (cons name engine)))))
  (find-diff-engine name))

(%register-engine :text (make-instance 'text-diff-engine))

;;; Lines

(defun split-lines (string)
  "→ fresh list of the lines of STRING, each with its #\\Newline terminator; a last line
without one is kept as is. \"\" → NIL. #\\Return stays part of the line (CRLF lines are
\"...\\r\\n\"). (apply #'concatenate 'string (split-lines s)) is STRING= to S."
  (check-type string string)
  (let ((out '())
        (n (length string))
        (start 0))
    (loop while (< start n)
          do (let* ((p (position #\Newline string :start start))
                    (end (if p (1+ p) n)))
               (push (subseq string start end) out)
               (setf start end)))
    (nreverse out)))

(defun %line-vector (string)
  (coerce (split-lines string) 'simple-vector))

(defun %join-lines (lines)
  (apply #'concatenate 'string lines))

(defun %has-newline-p (line)
  (and (plusp (length line)) (char= #\Newline (char line (1- (length line))))))

;;; Myers (minimal, O(ND)) and histogram, over index ranges of line vectors.
;;; Both return ascending matched pairs ((i . j) ...).

(defun %myers (av bv as ae bs be)
  (let* ((n (- ae as))
         (m (- be bs))
         (max (+ n m))
         (off (1+ max))
         (v (make-array (+ 3 (* 2 max)) :initial-element 0))
         (trace '()))
    (when (or (zerop n) (zerop m))
      (return-from %myers '()))
    (flet ((same (x y) (string= (svref av (+ as x)) (svref bv (+ bs y))))
           (down-p (v k d)
             (or (= k (- d))
                 (and (/= k d) (< (aref v (+ off k -1)) (aref v (+ off k 1)))))))
      (loop for d from 0 to max
            do (push (copy-seq v) trace)
               (loop for k from (- d) to d by 2
                     do (let* ((x (if (down-p v k d)
                                      (aref v (+ off k 1))
                                      (1+ (aref v (+ off k -1)))))
                               (y (- x k)))
                          (loop while (and (< x n) (< y m) (same x y))
                                do (incf x) (incf y))
                          (setf (aref v (+ off k)) x)
                          (when (and (>= x n) (>= y m))
                            ;; Backtrack through the snapshots (each taken before step D).
                            (let ((pairs '())
                                  (x n)
                                  (y m))
                              (loop for dd from d downto 1
                                    for snap in trace
                                    do (let* ((k (- x y))
                                              (pk (if (down-p snap k dd) (1+ k) (1- k)))
                                              (px (aref snap (+ off pk)))
                                              (py (- px pk)))
                                         (loop while (and (> x px) (> y py)
                                                          (> x (if (= pk (1+ k)) px (1+ px))))
                                               do (decf x) (decf y)
                                                  (push (cons (+ as x) (+ bs y)) pairs))
                                         (setf x px y py)))
                              (loop while (and (> x 0) (> y 0))
                                    do (decf x) (decf y)
                                       (push (cons (+ as x) (+ bs y)) pairs))
                              (return-from %myers pairs)))))))))

(defun %histogram (av bv as ae bs be)
  "git's histogram: anchor on the lowest-occurrence common line, extend the match both
ways, recurse on both sides; Myers where no line is unique enough."
  (when (or (= as ae) (= bs be))
    (return-from %histogram '()))
  (let ((counts (make-hash-table :test #'equal))
        (best nil))                     ; (rc len i0 j0)
    (loop for i from as below ae do (incf (gethash (svref av i) counts 0)))
    (loop for j from bs below be
          for c = (gethash (svref bv j) counts)
          when (and c (<= c 64))
            do (loop for i from as below ae
                     when (string= (svref av i) (svref bv j))
                       do (let ((i0 i) (j0 j) (i1 (1+ i)) (j1 (1+ j)))
                            (loop while (and (> i0 as) (> j0 bs)
                                             (string= (svref av (1- i0)) (svref bv (1- j0))))
                                  do (decf i0) (decf j0))
                            (loop while (and (< i1 ae) (< j1 be)
                                             (string= (svref av i1) (svref bv j1)))
                                  do (incf i1) (incf j1))
                            (let ((rc (loop for k from i0 below i1
                                            minimize (gethash (svref av k) counts)))
                                  (len (- i1 i0)))
                              (when (or (null best)
                                        (< rc (first best))
                                        (and (= rc (first best))
                                             (or (> len (second best))
                                                 (and (= len (second best))
                                                      (or (< j0 (fourth best))
                                                          (and (= j0 (fourth best))
                                                               (< i0 (third best))))))))
                                (setf best (list rc len i0 j0)))))))
    (if (null best)
        (%myers av bv as ae bs be)
        (destructuring-bind (rc len i0 j0) best
          (declare (ignore rc))
          (append (%histogram av bv as i0 bs j0)
                  (loop for k from 0 below len collect (cons (+ i0 k) (+ j0 k)))
                  (%histogram av bv (+ i0 len) ae (+ j0 len) be))))))

(defun %matches (algorithm av bv)
  (ecase algorithm
    (:myers (%myers av bv 0 (length av) 0 (length bv)))
    (:histogram (%histogram av bv 0 (length av) 0 (length bv)))))

(defun %runs (pairs la lb)
  "Changed runs (i0 i1 j0 j1), ascending: the maximal gaps between matched pairs."
  (let ((i 0) (j 0) (out '()))
    (dolist (p (append pairs (list (cons la lb))) (nreverse out))
      (when (or (< i (car p)) (< j (cdr p)))
        (push (list i (car p) j (cdr p)) out))
      (setf i (1+ (car p)) j (1+ (cdr p))))))

(defun %line-runs (algorithm av bv)
  (%runs (%matches algorithm av bv) (length av) (length bv)))

(defun %lcs-length (av bv)
  "Longest common subsequence length of two line vectors (Myers is minimal)."
  (length (%myers av bv 0 (length av) 0 (length bv))))

(defun %context-runs (runs n la)
  "Runs widened by N unchanged lines each way (clipped), merged when the unchanged gap
between two runs is <= 2N (git). Lines outside the runs match one to one, so the
widening is the same on both sides."
  (let ((groups '()))
    (dolist (r runs)
      (if (and groups (<= (- (first r) (second (first groups))) (* 2 n)))
          (let ((g (first groups)))
            (setf (second g) (second r) (fourth g) (fourth r)))
          (push (copy-list r) groups)))
    (mapcar (lambda (g)
              (destructuring-bind (i0 i1 j0 j1) g
                (let ((lead (min n i0))
                      (trail (min n (- la i1))))
                  (list (- i0 lead) (+ i1 trail) (- j0 lead) (+ j1 trail)))))
            (nreverse groups))))

;;; Hunks

(defun %text-fp (start end)
  (format nil "text:/~D-~D" start end))

(defun %make-hunk (p before after)
  "A hunk change at (P) replacing line list BEFORE by AFTER."
  (apply #'make-change (cond ((null before) :add) ((null after) :remove) (t :modify))
         :path (list p)
         :footprint (list (%text-fp p (+ p (length before))))
         (append (when before (list :before before))
                 (when after (list :after after)))))

(defun %vector-lines (v start end)
  (coerce (subseq v start end) 'list))

(defun %text-hunks (e a b context)
  "Hunks of A → B, descending."
  (let* ((av (%line-vector a))
         (bv (%line-vector b))
         (runs (%line-runs (text-engine-algorithm e) av bv))
         (runs (if (plusp context) (%context-runs runs context (length av)) runs)))
    (reverse (mapcar (lambda (r)
                       (destructuring-bind (i0 i1 j0 j1) r
                         (%make-hunk i0 (%vector-lines av i0 i1) (%vector-lines bv j0 j1))))
                     runs))))

(defun %check-text (e x)
  (unless (stringp x)
    (error 'unsupported-content :engine e :content x)))

(defmethod state-equal ((e text-diff-engine) a b)
  (and (stringp a) (stringp b) (string= a b)))

(defmethod compute-diff ((e text-diff-engine) a b &key (level :changes) (context 0))
  (check-type level diff-level)
  (check-type context (integer 0))
  (%check-text e a)
  (%check-text e b)
  (make-diff
   (cond ((string= a b) '())
         ((eq level :summary)
          (list (make-change :modify :footprint (list "text:/")
                                     :before-fn (lambda () a) :after-fn (lambda () b))))
         ((eq level :content)
          (list (make-change :modify :footprint (list "text:/") :before a :after b
                                     :children (%text-hunks e a b context))))
         (t (%text-hunks e a b context)))
   :engine e :level level))

;;; Apply

(defmethod %apply-change ((e text-diff-engine) state (c opaque-change) d)
  (declare (ignore state))
  (error 'unsupported-content :engine e :content c :change c :diff d))

(defmethod %apply-change ((e text-diff-engine) state (c change) d)
  (unless (stringp state)
    (error 'unsupported-content :engine e :content state :change c :diff d))
  (if (null (change-path c))
      (if (string= state (change-before c))
          (change-after c)
          (error 'apply-conflict :reason :before-mismatch :actual state :change c :diff d))
      (let* ((lines (split-lines state))
             (p (first (change-path c)))
             (before (change-before c))
             (end (+ p (length before))))
        (unless (and (integerp p) (<= 0 p) (<= end (length lines)))
          (error 'apply-conflict :reason :missing :actual nil :change c :diff d))
        (let ((found (subseq lines p end)))
          (unless (every #'string= found before)
            (error 'apply-conflict :reason :before-mismatch :actual found :change c :diff d))
          (%join-lines (append (subseq lines 0 p) (change-after c) (nthcdr end lines)))))))

(defmethod %put-value ((e text-diff-engine) state (c change) value d)
  (unless (stringp state)
    (error 'unsupported-content :engine e :content state :change c :diff d))
  (let ((value-string (if (stringp value) value (%join-lines value))))
    (if (null (change-path c))
        value-string
        (let* ((lines (split-lines state))
               (n (length lines))
               (p (min n (max 0 (first (change-path c)))))
               (end (min n (+ p (length (change-before c))))))
          (%join-lines (append (subseq lines 0 p) (split-lines value-string)
                               (nthcdr end lines)))))))

;;; diff3

(defun %diff3-regions (e base ours theirs)
  "→ ascending regions (start end status ours-lines theirs-lines) over BASE's lines;
STATUS :OURS, :THEIRS, :BOTH (same lines) or :CONFLICT. Two hunks [s1,e1) [s2,e2)
interact when s1 <= e2 and s2 <= e1; regions close that relation transitively."
  (%check-text e base)
  (%check-text e ours)
  (%check-text e theirs)
  (let* ((algorithm (text-engine-algorithm e))
         (bv (%line-vector base))
         (ov (%line-vector ours))
         (tv (%line-vector theirs))
         (hunks (stable-sort
                 (append (mapcar (lambda (r) (cons :ours r)) (%line-runs algorithm bv ov))
                         (mapcar (lambda (r) (cons :theirs r)) (%line-runs algorithm bv tv)))
                 #'< :key #'second))
         (groups '()))
    ;; Sorted by start: a hunk joins the open region iff its start <= the region's end.
    (dolist (h hunks)
      (if (and groups (<= (second h) (second (first groups))))
          (let ((g (first groups)))
            (setf (second g) (max (second g) (third h)))
            (push h (third g)))
          (push (list (second h) (third h) (list h)) groups)))
    (flet ((side-lines (side start end members vec)
             (let ((pos start) (out '()))
               (dolist (h (reverse members))
                 (when (eq (first h) side)
                   (destructuring-bind (i0 i1 j0 j1) (rest h)
                     (setf out (append out (%vector-lines bv pos i0) (%vector-lines vec j0 j1))
                           pos i1))))
               (append out (%vector-lines bv pos end)))))
      (mapcar (lambda (g)
                (destructuring-bind (start end members) g
                  (let* ((has-ours (find :ours members :key #'first))
                         (has-theirs (find :theirs members :key #'first))
                         (o (side-lines :ours start end members ov))
                         (x (side-lines :theirs start end members tv)))
                    (list start end
                          (cond ((not has-theirs) :ours)
                                ((not has-ours) :theirs)
                                ((equal o x) :both)
                                (t :conflict))
                          o x))))
              (nreverse groups)))))

(defmethod %three-way ((e text-diff-engine) base ours theirs resolve)
  (let* ((bv (%line-vector base))
         (merged '())
         (conflicts '()))
    (dolist (r (%diff3-regions e base ours theirs))
      (destructuring-bind (start end status o x) r
        (let ((before (%vector-lines bv start end)))
          (ecase status
            ((:ours :both) (push (%make-hunk start before o) merged))
            (:theirs (push (%make-hunk start before x) merged))
            (:conflict
             (push (%conflict (%make-hunk start before o) (%make-hunk start before x))
                   conflicts)
             (when (eq resolve :theirs)
               (push (%make-hunk start before x) merged)))))))
    ;; MERGED is descending (pushed in ascending order).
    (values (make-diff merged :engine e :level :changes)
            (nreverse conflicts))))

(defmethod %rebase ((e text-diff-engine) d onto base)
  (%rebase-by-merge e d onto base))

(defun merge-texts (base ours theirs &key engine (style :merge)
                                          (ours-label "ours") (base-label "base")
                                          (theirs-label "theirs"))
  "Three-way merge of strings → (values merged-string conflicts). CONFLICTS is the
second value of THREE-WAY-DIFF. Each conflict region is written with markers
(\"<<<<<<< ours\", STYLE :DIFF3 adds \"||||||| base\", \"=======\", \">>>>>>> theirs\").
ENGINE defaults to (find-diff-engine :text)."
  (check-type style (member :merge :diff3))
  (let* ((e (or engine (find-diff-engine :text)))
         (bv (%line-vector base))
         (regions (%diff3-regions e base ours theirs))
         (conflicts (nth-value 1 (three-way-diff e base ours theirs))))
    (values
     (with-output-to-string (s)
       (flet ((lines (ls)
                (dolist (l ls) (write-string l s))
                (let ((last (car (last ls))))
                  (when (and last (not (%has-newline-p last)))
                    (terpri s)))))
         (let ((pos 0))
           (dolist (r regions)
             (destructuring-bind (start end status o x) r
               (dolist (l (%vector-lines bv pos start)) (write-string l s))
               (ecase status
                 ((:ours :both) (dolist (l o) (write-string l s)))
                 (:theirs (dolist (l x) (write-string l s)))
                 (:conflict
                  (format s "<<<<<<< ~A~%" ours-label)
                  (lines o)
                  (when (eq style :diff3)
                    (format s "||||||| ~A~%" base-label)
                    (lines (%vector-lines bv start end)))
                  (format s "=======~%")
                  (lines x)
                  (format s ">>>>>>> ~A~%" theirs-label)))
               (setf pos end)))
           (dolist (l (%vector-lines bv pos (length bv))) (write-string l s)))))
     conflicts)))
