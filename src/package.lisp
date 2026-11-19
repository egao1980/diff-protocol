(defpackage #:diff-protocol
  (:use #:cl)
  (:nicknames #:stack-diff)
  (:export
   ;; conditions
   #:diff-error #:diff-error-diff #:diff-error-change
   #:irreversible-change
   #:apply-conflict #:apply-conflict-actual #:apply-conflict-reason
   #:unsupported-content #:unsupported-content-engine #:unsupported-content-content
   ;; restarts (skip-change is also a function, like CL:CONTINUE)
   #:skip-change
   ;; changes
   #:change #:make-change
   #:change-kind #:change-path #:change-from #:change-footprint #:change-children
   #:change-before #:change-after
   #:change-before-loaded-p #:change-after-loaded-p
   #:opaque-change #:make-opaque-change #:opaque-change-description
   ;; diffs
   #:diff #:make-diff #:diff-changes #:diff-level #:diff-engine
   #:diff-footprint #:diff-empty-p
   #:footprint-overlap-p
   ;; conflicts (data)
   #:diff-conflict #:conflict-ours #:conflict-theirs #:conflict-footprint
   ;; engines
   ;; diff-engine (exported above) is also the engine base class
   #:sexp-diff-engine #:engine-name #:find-diff-engine
   ;; protocol
   #:compute-diff #:state-equal #:apply-diff #:invert-diff #:compose-diffs
   #:rebase-diff #:three-way-diff #:render-diff #:parse-diff
   ;; R1: parse errors
   #:diff-parse-error #:diff-parse-error-line
   ;; R1: text engine
   #:text-diff-engine #:text-engine-algorithm #:split-lines #:merge-texts
   ;; R1: tree values (plain data). TREE-MODE is the path step that addresses a file's mode.
   #:make-tree #:make-tree-file #:tree-dir-p #:tree-file-p #:tree-entries
   #:tree-file-type #:tree-file-content #:tree-file-mode #:tree-get #:tree-mode
   ;; R1: tree engine
   #:tree-diff-engine #:tree-engine-rename-threshold #:tree-engine-detect-renames
   #:tree-engine-leaf-engines))
