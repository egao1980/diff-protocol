(in-package #:diff-protocol)

;;; R1 text engine (brief: "R1 — locked", "Text engine").
;;; CONTRACT SKELETON: the class and stubs only. The implementation (Myers,
;;; histogram, hunks, diff3, MERGE-TEXTS, engine registration under :TEXT,
;;; COMPUTE-DIFF / STATE-EQUAL / %APPLY-CHANGE / %PUT-VALUE / THREE-WAY-DIFF
;;; methods, inverse hunk footprints, text REBASE-DIFF) is team-solve's job.

(defun %r1-todo (what)
  (error "diff-protocol R1: ~A is not implemented yet" what))

(defclass text-diff-engine (diff-engine)
  ((algorithm :initarg :algorithm :initform :myers :reader text-engine-algorithm))
  (:documentation "Line-based engine for strings. ENGINE-NAME → :TEXT.
ALGORITHM is :MYERS (minimal, default) or :HISTOGRAM; anything else → TYPE-ERROR
from MAKE-INSTANCE."))

(defun split-lines (string)
  "→ fresh list of the lines of STRING, each with its #\\Newline terminator; a last line
without one is kept as is. \"\" → NIL. #\\Return stays part of the line (CRLF lines are
\"...\\r\\n\"). (apply #'concatenate 'string (split-lines s)) is STRING= to S."
  (declare (ignore string))
  (%r1-todo 'split-lines))

(defun merge-texts (base ours theirs &key engine (style :merge)
                                          (ours-label "ours") (base-label "base")
                                          (theirs-label "theirs"))
  "Three-way merge of strings → (values merged-string conflicts). CONFLICTS is the
second value of THREE-WAY-DIFF. Each conflict region is written with markers
(\"<<<<<<< ours\", STYLE :DIFF3 adds \"||||||| base\", \"=======\", \">>>>>>> theirs\").
ENGINE defaults to (find-diff-engine :text)."
  (declare (ignore base ours theirs engine style ours-label base-label theirs-label))
  (%r1-todo 'merge-texts))
