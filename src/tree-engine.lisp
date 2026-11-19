(in-package #:diff-protocol)

;;; R1 tree engine (brief: "Tree engine"). CONTRACT SKELETON: the class only.
;;; Methods (COMPUTE-DIFF with rename detection, STATE-EQUAL, %APPLY-CHANGE,
;;; %PUT-VALUE, THREE-WAY-DIFF, tree REBASE-DIFF) and registration under :TREE
;;; are team-solve's job.

(defclass tree-diff-engine (diff-engine)
  ((rename-threshold :initarg :rename-threshold :initform 0.5
                     :reader tree-engine-rename-threshold)
   (detect-renames :initarg :detect-renames :initform t
                   :reader tree-engine-detect-renames)
   (leaf-engines :initarg :leaf-engines :initform '((:text . :text) (:sexp . :sexp))
                 :reader tree-engine-leaf-engines))
  (:documentation "Engine for tree values (MAKE-TREE). ENGINE-NAME → :TREE.
LEAF-ENGINES: alist file type → engine name (via FIND-DIFF-ENGINE) or engine instance.
RENAME-THRESHOLD: line similarity in [0, 1] at or above which a removed and an added
text file pair up as a :MOVE. DETECT-RENAMES NIL turns rename detection off."))
