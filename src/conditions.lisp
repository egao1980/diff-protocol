(in-package #:diff-protocol)

;;; Condition tree (brief: cl-stack/docs/capabilities/diff.md, "Conditions").
;;; Restarts are established by the signalling GFs, not here:
;;;   apply-diff  → apply-conflict:      skip-change, use-value, abort
;;;   invert-diff → irreversible-change: skip-change, use-value

(define-condition diff-error (error)
  ((diff :initarg :diff :initform nil :reader diff-error-diff)
   (change :initarg :change :initform nil :reader diff-error-change))
  (:report (lambda (c s)
             (format s "Diff error~@[ on change ~S~]" (diff-error-change c))))
  (:documentation "Root of the diff-protocol condition tree."))

(define-condition irreversible-change (diff-error)
  ()
  (:report (lambda (c s)
             (format s "Change has no inverse: ~S" (diff-error-change c))))
  (:documentation "INVERT-DIFF met a change with no inverse (e.g. an OPAQUE-CHANGE)."))

(define-condition apply-conflict (diff-error)
  ((actual :initarg :actual :initform nil :reader apply-conflict-actual)
   (reason :initarg :reason :initform nil :reader apply-conflict-reason))
  (:report (lambda (c s)
             (format s "Cannot apply change ~S (~(~A~)); found ~S"
                     (diff-error-change c) (apply-conflict-reason c)
                     (apply-conflict-actual c))))
  (:documentation "APPLY-DIFF found a state that does not match a change.
REASON is :MISSING, :EXISTS or :BEFORE-MISMATCH; ACTUAL is the value found at the path, or NIL."))

(define-condition unsupported-content (diff-error)
  ((engine :initarg :engine :initform nil :reader unsupported-content-engine)
   (content :initarg :content :initform nil :reader unsupported-content-content))
  (:report (lambda (c s)
             (format s "~@[Engine ~S: ~]unsupported content ~S"
                     (unsupported-content-engine c) (unsupported-content-content c))))
  (:documentation "An engine cannot diff or apply this content (wrong type, opaque change)."))

(define-condition diff-parse-error (diff-error)
  ((line :initarg :line :initform nil :reader diff-parse-error-line)
   (text :initarg :text :initform nil :reader %diff-parse-error-text))
  (:report (lambda (c s)
             (format s "Cannot parse diff~@[ at line ~D~]~@[: ~S~]"
                     (diff-parse-error-line c) (%diff-parse-error-text c))))
  (:documentation "PARSE-DIFF met malformed input. LINE is the 1-based input line number."))

(defun skip-change(&optional condition)
  "Invoke the SKIP-CHANGE restart (of CONDITION, if given). Like CL:CONTINUE: returns NIL when there is none."
  (let ((restart (find-restart 'skip-change condition)))
    (when restart
      (invoke-restart restart))))
