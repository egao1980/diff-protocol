(in-package #:diff-protocol)

;;; Protocol layer: value classes, constructors, readers, engine registry and
;;; the generic functions. The sexp engine is in sexp-engine.lisp, the
;;; engine-independent operations in ops.lisp, render / parse in render.lisp.
;;; Brief: cl-stack/docs/capabilities/diff.md.

(defmacro %define-alias (old new)
  "Make OLD name the same function as NEW (a 0.2 internal name kept working)."
  `(setf (fdefinition ',old) (fdefinition ',new)))

(defstruct (%lazy (:constructor %lazy (value fn loaded-p)) (:copier nil))
  "One side (before / after) of a change: a value, or a thunk forced once and cached.
Shared between a change and its inverse; the only mutation in the protocol is %FORCE."
  value
  (fn nil :type (or null function))
  (loaded-p nil))

;;; Types (CHANGE-KIND and DIFF-LEVEL also name readers below)

(deftype change-kind () '(member :add :remove :modify :move))
(deftype diff-level () '(member :summary :changes :content))

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

;;; Values

(defclass change ()
  ((kind :initarg :kind :reader change-kind)
   (path :initarg :path :initform nil :reader change-path)
   (from :initarg :from :initform nil :reader change-from)
   (footprint :initarg :footprint :initform nil :reader change-footprint)
   (children :initarg :children :initform nil :reader change-children)
   (before :initarg :before-box :initform (%lazy nil nil nil) :reader %before-box)
   (after :initarg :after-box :initform (%lazy nil nil nil) :reader %after-box))
  (:documentation "One change: kind, path, from (moves), footprint, lazy before/after, children."))

(defclass opaque-change (change)
  ((description :initarg :description :initform nil :reader opaque-change-description))
  (:documentation "Footprint known, content not (e.g. a migration step with arbitrary code)."))

(defclass diff ()
  ((changes :initarg :changes :initform nil :reader diff-changes)
   (level :initarg :level :initform :changes :reader diff-level)
   (engine :initarg :engine :initform nil :reader diff-engine))
  (:documentation "A diff value: changes, level, engine."))

(defclass diff-conflict ()
  ((ours :initarg :ours :reader conflict-ours)
   (theirs :initarg :theirs :reader conflict-theirs)
   (footprint :initarg :footprint :initform nil :reader conflict-footprint))
  (:documentation "Data, not a signal: one overlapping (ours, theirs) pair from REBASE-DIFF / THREE-WAY-DIFF."))

(defclass diff-engine ()
  ()
  (:documentation "Base class of engines; one subclass per content type."))

(defclass sexp-diff-engine (diff-engine)
  ()
  (:documentation "Engine for atoms, plists and lists. ENGINE-NAME → :SEXP."))

(defun make-change (kind &key path from footprint (before nil before-p) (after nil after-p)
                         before-fn after-fn children)
  "→ CHANGE. KIND must be of type CHANGE-KIND (else TYPE-ERROR).
BEFORE-FN / AFTER-FN are thunks forced once, on first read. A side given as a value
is loaded; a side given neither as a value nor as a thunk reads NIL and is not loaded.
A value wins over a thunk for the same side."
  (check-type kind change-kind)
  (check-type before-fn (or null function))
  (check-type after-fn (or null function))
  (check-type footprint list)
  (check-type children list)
  (make-instance 'change :kind kind :path path :from from
                         :footprint (copy-list footprint)
                         :children (copy-list children)
                         :before-box (%lazy before (unless before-p before-fn) before-p)
                         :after-box (%lazy after (unless after-p after-fn) after-p)))

(defun make-opaque-change (kind &key footprint description)
  "→ OPAQUE-CHANGE. KIND must be of type CHANGE-KIND (else TYPE-ERROR)."
  (check-type kind change-kind)
  (check-type footprint list)
  (make-instance 'opaque-change :kind kind :footprint (copy-list footprint)
                                :description description))

(defun make-diff (changes &key engine (level :changes))
  "→ DIFF over CHANGES (a list). LEVEL must be of type DIFF-LEVEL."
  (check-type changes list)
  (check-type level diff-level)
  (check-type engine (or null diff-engine))
  (make-instance 'diff :changes (copy-list changes) :engine engine :level level))

(defun footprint-overlap-p (f1 f2)
  "True when resource strings F1 and F2 are STRING=, or one is a prefix of the other
ending at a \"/\" boundary (the prefix ends in \"/\" or the next char is \"/\")."
  (check-type f1 string)
  (check-type f2 string)
  (multiple-value-bind (short long)
      (if (<= (length f1) (length f2)) (values f1 f2) (values f2 f1))
    (let ((n (length short)))
      (or (string= short long)
          (and (< n (length long))
               (string= short long :end2 n)
               (or (and (plusp n) (char= (char short (1- n)) #\/))
                   (char= (char long n) #\/)))))))

(defvar *diff-engines* (list (cons :sexp (make-instance 'sexp-diff-engine)))
  "Alist keyword → engine instance (one shared instance per engine).")

(defun find-diff-engine (name)
  "→ the engine instance registered under keyword NAME (:SEXP at R0), or NIL."
  (cdr (assoc name *diff-engines*)))

(defun register-diff-engine (name engine)
  "Register ENGINE (a DIFF-ENGINE) under keyword NAME so FIND-DIFF-ENGINE and
PARSE-DIFF :SEXP find it → the registered instance. A name is registered once: when
NAME is taken, the existing shared instance is kept and returned (a reload keeps it).
NAME should be ENGINE's ENGINE-NAME."
  (check-type name keyword)
  (check-type engine diff-engine)
  (unless (find-diff-engine name)
    (setf *diff-engines* (append *diff-engines* (list (cons name engine)))))
  (find-diff-engine name))

(%define-alias %register-engine register-diff-engine)

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

;;; Engine extension API (diff-protocol#6). An engine in any repo subclasses
;;; DIFF-ENGINE, registers one shared instance with REGISTER-DIFF-ENGINE and
;;; specializes the GFs below; APPLY-DIFF (restarts), INVERT-DIFF, COMPOSE-DIFFS,
;;; THREE-WAY-DIFF, REBASE-DIFF, the tree engine's nesting and the renders then
;;; work unchanged. The 0.2 internal names (%APPLY-CHANGE, %PUT-VALUE,
;;; %INVERT-CHANGE, %THREE-WAY, %REBASE, %REGISTER-ENGINE) name the same
;;; functions, so methods defined on either name are one method set.

(defgeneric apply-change (engine state change diff)
  (:documentation "Engine hook of APPLY-DIFF: → new state after one leaf CHANGE of DIFF.
Signals APPLY-CONFLICT (:REASON :MISSING / :EXISTS / :BEFORE-MISMATCH) or
UNSUPPORTED-CONTENT; APPLY-DIFF wraps each call in its SKIP-CHANGE / USE-VALUE / ABORT
restarts. Never mutates STATE."))

(%define-alias %apply-change apply-change)

(defgeneric invert-change (change diff)
  (:documentation "Hook of INVERT-DIFF: → the inverse of CHANGE (of DIFF). The CHANGE
method swaps kind (:ADD ↔ :REMOVE), path / from of a :MOVE and the lazy sides, and
keeps footprints (\"text:…/s-e\" ranges are recomputed); an engine with its own
change subclass specializes on it. Signals IRREVERSIBLE-CHANGE when there is none."))

(%define-alias %invert-change invert-change)

(defgeneric put-value (engine state change value diff)
  (:documentation "Engine hook of APPLY-DIFF's USE-VALUE restart: → new state with VALUE
at CHANGE's path. Signals APPLY-CONFLICT (:MISSING) when the path cannot be reached."))

(%define-alias %put-value put-value)

(defgeneric render-unified-content (engine before after changes stream)
  (:documentation "Engine hook of RENDER-DIFF :UNIFIED (diff-protocol#7): write the
@@ hunks (no ---/+++ header) of one content BEFORE → AFTER to STREAM and return true,
or return NIL when the engine has no unified form (the tree renders the file as
`Binary files … differ`; a diff of the engine itself signals UNSUPPORTED-CONTENT).
BEFORE / AFTER are the contents when the diff carries them, else NIL (an :ADD has no
BEFORE, a :REMOVE no AFTER, a :CHANGES-level tree diff carries neither). CHANGES are
the engine's own changes for that content (paths relative to it), NIL for a whole-content
section. The DIFF-ENGINE method returns NIL; the text engine's renders line hunks
(its CHANGES when given, else a line diff of BEFORE / AFTER, NIL read as \"\") — an
engine whose content is text can delegate to (find-diff-engine :text)."))

(defmethod render-unified-content ((e diff-engine) before after changes stream)
  (declare (ignore before after changes stream))
  nil)

(defun %force (box)
  "Value of BOX; forces and caches its thunk. An absent side reads NIL and stays not loaded."
  (cond ((%lazy-loaded-p box) (%lazy-value box))
        ((%lazy-fn box)
         (let ((v (funcall (%lazy-fn box))))
           (setf (%lazy-value box) v
                 (%lazy-loaded-p box) t
                 (%lazy-fn box) nil)
           v))
        (t nil)))

(defmethod change-before ((c change)) (%force (%before-box c)))

(defmethod change-after ((c change)) (%force (%after-box c)))

(defmethod change-before-loaded-p ((c change)) (and (%lazy-loaded-p (%before-box c)) t))

(defmethod change-after-loaded-p ((c change)) (and (%lazy-loaded-p (%after-box c)) t))

(defmethod print-object ((c change) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (format stream "~S~{ ~A~}" (change-kind c) (change-footprint c))))

(defun %leaves (changes)
  "Leaf changes of CHANGES, depth-first: a change with children is replaced by their leaves."
  (loop for c in changes
        append (if (change-children c) (%leaves (change-children c)) (list c))))

(defun %sorted-footprint (lists)
  "Fresh list: union of the footprint LISTS, deduped with STRING=, sorted STRING<."
  (let ((acc '()))
    (dolist (l lists)
      (dolist (f l) (pushnew f acc :test #'string=)))
    (sort acc #'string<)))

(defmethod diff-footprint ((d diff))
  (let ((lists '()))
    (labels ((walk (c)
               (push (change-footprint c) lists)
               (mapc #'walk (change-children c))))
      (mapc #'walk (diff-changes d)))
    (%sorted-footprint lists)))

(defmethod diff-empty-p ((d diff))
  (null (diff-changes d)))

(defmethod engine-name ((e sexp-diff-engine))
  :sexp)
