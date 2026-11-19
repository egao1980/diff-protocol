(in-package #:diff-protocol)

;;; R0 contract skeleton. Classes, types and generic functions are declared;
;;; no methods exist yet. Slots, methods and the sexp engine are the R0
;;; team-solve work. Brief: cl-stack/docs/capabilities/diff.md.

(defun %unimplemented (name)
  (error "~S is not implemented yet (diff-protocol R0 contract skeleton)" name))

;;; Types (CHANGE-KIND and DIFF-LEVEL also name readers below)

(deftype change-kind () '(member :add :remove :modify :move))
(deftype diff-level () '(member :summary :changes :content))

;;; Values

(defclass change ()
  ()
  (:documentation "One change: kind, path, from (moves), footprint, lazy before/after, children."))

(defclass opaque-change (change)
  ()
  (:documentation "Footprint known, content not (e.g. a migration step with arbitrary code)."))

(defclass diff ()
  ()
  (:documentation "A diff value: changes, level, engine."))

(defclass diff-conflict ()
  ()
  (:documentation "Data, not a signal: one overlapping (ours, theirs) pair from REBASE-DIFF / THREE-WAY-DIFF."))

(defclass diff-engine ()
  ()
  (:documentation "Base class of engines; one subclass per content type."))

(defclass sexp-diff-engine (diff-engine)
  ()
  (:documentation "Engine for atoms, plists and lists. ENGINE-NAME → :SEXP."))

(defun make-change (kind &key path from footprint before after before-fn after-fn children)
  "→ CHANGE. KIND must be of type CHANGE-KIND (else TYPE-ERROR).
BEFORE-FN / AFTER-FN are thunks forced once, on first read."
  (declare (ignore kind path from footprint before after before-fn after-fn children))
  (%unimplemented 'make-change))

(defun make-opaque-change (kind &key footprint description)
  "→ OPAQUE-CHANGE."
  (declare (ignore kind footprint description))
  (%unimplemented 'make-opaque-change))

(defun make-diff (changes &key engine (level :changes))
  "→ DIFF over CHANGES (a list). LEVEL must be of type DIFF-LEVEL."
  (declare (ignore changes engine level))
  (%unimplemented 'make-diff))

(defun footprint-overlap-p (f1 f2)
  "True when resource strings F1 and F2 are STRING=, or one is a prefix of the other
ending at a \"/\" boundary (the prefix ends in \"/\" or the next char is \"/\")."
  (declare (ignore f1 f2))
  (%unimplemented 'footprint-overlap-p))

(defun find-diff-engine (name)
  "→ the engine instance registered under keyword NAME (:SEXP at R0), or NIL."
  (declare (ignore name))
  (%unimplemented 'find-diff-engine))

;;; Readers

(defgeneric change-kind (change)
  (:documentation "One of :ADD :REMOVE :MODIFY :MOVE."))
(defgeneric change-path (change)
  (:documentation "Steps from the state root (keywords, indices); NIL = root."))
(defgeneric change-from (change)
  (:documentation "Source path of a :MOVE; NIL otherwise."))
(defgeneric change-footprint (change)
  (:documentation "List of resource strings."))
(defgeneric change-children (change)
  (:documentation "Sub-changes (absolute paths) of a :MODIFY at level :CONTENT."))
(defgeneric change-before (change)
  (:documentation "Before content; forces and caches a lazy thunk. NIL for :ADD."))
(defgeneric change-after (change)
  (:documentation "After content; forces and caches a lazy thunk. NIL for :REMOVE."))
(defgeneric change-before-loaded-p (change)
  (:documentation "True once BEFORE is a value (given or forced)."))
(defgeneric change-after-loaded-p (change)
  (:documentation "True once AFTER is a value (given or forced)."))
(defgeneric opaque-change-description (change)
  (:documentation "Human-readable description of an OPAQUE-CHANGE."))

(defgeneric diff-changes (diff)
  (:documentation "The list of top-level changes."))
(defgeneric diff-level (diff)
  (:documentation "One of :SUMMARY :CHANGES :CONTENT."))
(defgeneric diff-engine (diff)
  (:documentation "The engine that computed the diff, or NIL."))
(defgeneric diff-footprint (diff)
  (:documentation "Union of the footprints of all changes and children, deduped, sorted STRING<."))
(defgeneric diff-empty-p (diff)
  (:documentation "True when the diff has no changes."))

(defgeneric conflict-ours (conflict)
  (:documentation "The change from OURS (from D in REBASE-DIFF)."))
(defgeneric conflict-theirs (conflict)
  (:documentation "The change from THEIRS (from ONTO in REBASE-DIFF)."))
(defgeneric conflict-footprint (conflict)
  (:documentation "Union of both changes' footprints, deduped, sorted STRING<."))

(defgeneric engine-name (engine)
  (:documentation "Keyword naming the engine (:SEXP)."))

;;; Protocol

(defgeneric compute-diff (engine a b &key level)
  (:documentation "→ DIFF from state A to state B. LEVEL defaults to :CHANGES.
Signals UNSUPPORTED-CONTENT for content the engine cannot diff."))

(defgeneric state-equal (engine a b)
  (:documentation "The state equality every law uses (sexp: plists order-insensitive)."))

(defgeneric apply-diff (state diff)
  (:documentation "Pure → (values new-state skipped-changes). Signals APPLY-CONFLICT with
restarts SKIP-CHANGE, USE-VALUE, ABORT (→ (values state :aborted))."))

(defgeneric invert-diff (diff)
  (:documentation "→ inverse DIFF. Signals IRREVERSIBLE-CHANGE (restarts SKIP-CHANGE, USE-VALUE)."))

(defgeneric compose-diffs (d1 d2)
  (:documentation "→ DIFF equivalent to applying D1 then D2."))

(defgeneric rebase-diff (diff onto base)
  (:documentation "DIFF and ONTO both against BASE → (values diff-after-onto conflicts)."))

(defgeneric three-way-diff (engine base ours theirs)
  (:documentation "→ (values merged-diff-against-base conflicts). Conflicts are DIFF-CONFLICT data."))

(defgeneric render-diff (diff format stream)
  (:documentation "FORMAT :SUMMARY | :SEXP. STREAM NIL → string, T → *STANDARD-OUTPUT*."))

(defgeneric parse-diff (format input)
  (:documentation "FORMAT :SEXP; INPUT a string or stream → DIFF."))
