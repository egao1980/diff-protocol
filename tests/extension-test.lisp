(in-package #:diff-protocol/tests)

;;; Engine extension API (diff-protocol#6, #7). NOTE-ENGINE is an engine defined
;;; outside diff-protocol with exported symbols only (single-colon D: names): its
;;; states are strings, a diff is one whole-content :MODIFY at path (:BODY).

(defclass note-engine (d:diff-engine)
  ((merges :initform 0 :accessor note-merges)
   (rebases :initform 0 :accessor note-rebases))
  (:documentation "Test engine: whole-string changes, rendered as text hunks."))

(defmethod d:engine-name ((e note-engine))
  :note)

(defun note-check (e x)
  (unless (stringp x)
    (error 'd:unsupported-content :engine e :content x)))

(defmethod d:state-equal ((e note-engine) a b)
  (and (stringp a) (stringp b) (string= a b)))

(defmethod d:compute-diff ((e note-engine) a b &key (level :changes) &allow-other-keys)
  (note-check e a)
  (note-check e b)
  (d:make-diff (unless (string= a b)
                 (list (d:make-change :modify :path '(:body) :footprint '("note:/")
                                              :before a :after b)))
               :engine e :level level))

(defmethod d:apply-change ((e note-engine) state (c d:change) diff)
  (unless (and (stringp state) (string= state (d:change-before c)))
    (error 'd:apply-conflict :reason :before-mismatch :actual state :change c :diff diff))
  (d:change-after c))

(defmethod d:put-value ((e note-engine) state (c d:change) value diff)
  (declare (ignore state diff))
  value)

(defmethod d:merge-states ((e note-engine) base ours theirs resolve)
  (declare (ignore base ours theirs resolve))
  (incf (note-merges e))
  (call-next-method))

(defmethod d:rebase-states ((e note-engine) diff onto base)
  (incf (note-rebases e))
  (d:rebase-by-merge e diff onto base))

(defmethod d:render-unified-content ((e note-engine) before after changes stream)
  ;; A whole-content change carries both sides; delegate the hunks to the text engine.
  (let ((c (first changes)))
    (d:render-unified-content (d:find-diff-engine :text)
                              (if c (d:change-before c) before)
                              (if c (d:change-after c) after)
                              nil stream)))

(defvar *note* (d:register-diff-engine :note (make-instance 'note-engine)))

(defvar *note-tree*
  (make-instance 'd:tree-diff-engine :leaf-engines '((:text . :text) (:note . :note))))

(defun note-tree (type &rest files)
  "Tree of FILES ((name content) ...) all of TYPE."
  (apply #'d:make-tree
         (loop for (name content) in files
               append (list name (d:make-tree-file content :type type)))))

(deftest extension-register
  (ok (typep *note* 'note-engine) "REGISTER-DIFF-ENGINE returns the instance")
  (ok (eq *note* (d:find-diff-engine :note)))
  (ok (eq *note* (d:register-diff-engine :note (make-instance 'note-engine)))
      "registering a name again keeps the shared instance")
  (ok (signals (d:register-diff-engine "note" *note*) 'type-error))
  (ok (signals (d:register-diff-engine :x 42) 'type-error)))

(deftest extension-compute-apply
  (let ((diff (d:compute-diff *note* (nl "a") (nl "b"))))
    (ok (eq *note* (d:diff-engine diff)))
    (ok (equal '("note:/") (d:diff-footprint diff)))
    (ok (equal (nl "b") (d:apply-diff (nl "a") diff)))
    (ok (equal (nl "a") (d:apply-diff (nl "b") (d:invert-diff diff))) "invert")
    (ok (equal (nl "c") (d:apply-diff (nl "a")
                                      (d:compose-diffs diff (d:compute-diff *note* (nl "b") (nl "c")))))
        "compose")
    (ok (equal (nl "M note:/") (d:render-diff diff :summary nil)))
    (let ((back (d:parse-diff :sexp (d:render-diff diff :sexp nil))))
      (ok (eq *note* (d:diff-engine back)) ":sexp parse finds the registered engine")
      (ok (equal (nl "b") (d:apply-diff (nl "a") back))))))

(deftest extension-apply-restarts
  (let ((diff (d:compute-diff *note* (nl "a") (nl "b"))))
    (ok (signals (d:apply-diff (nl "z") diff) 'd:apply-conflict))
    (multiple-value-bind (state skipped)
        (handler-bind ((d:apply-conflict #'d:skip-change))
          (d:apply-diff (nl "z") diff))
      (ok (equal (nl "z") state) "skip-change")
      (ok (= 1 (length skipped))))
    (ok (equal "put" (handler-bind ((d:apply-conflict (lambda (c) (use-value "put" c))))
                       (d:apply-diff (nl "z") diff)))
        "use-value goes through PUT-VALUE")))

(deftest extension-merge-rebase
  (let ((merges (note-merges *note*))
        (rebases (note-rebases *note*))
        (base (nl "a")))
    (multiple-value-bind (merged conflicts) (d:three-way-diff *note* base (nl "b") (nl "b"))
      (ok (null conflicts) "same change on both sides")
      (ok (equal (nl "b") (d:apply-diff base merged))))
    (multiple-value-bind (merged conflicts) (d:three-way-diff *note* base (nl "b") (nl "c"))
      (ok (= 1 (length conflicts)))
      (ok (equal '("note:/") (d:conflict-footprint (first conflicts))))
      (ok (equal base (d:apply-diff base merged)) "conflicting side left out"))
    (ok (< merges (note-merges *note*)) "THREE-WAY-DIFF dispatches to MERGE-STATES")
    (multiple-value-bind (rebased conflicts)
        (d:rebase-diff (d:compute-diff *note* base (nl "b")) (d:compute-diff *note* base (nl "c")) base)
      (ok (= 1 (length conflicts)))
      (ok (d:diff-empty-p rebased) "conflict resolved to ONTO"))
    (ok (< rebases (note-rebases *note*)) "REBASE-DIFF dispatches to REBASE-STATES")))

(deftest extension-render-unified
  (let ((expected (nl "@@ -1 +1 @@" "-a" "+b")))
    (ok (equal expected (d:render-diff (d:compute-diff *note* (nl "a") (nl "b")) :unified nil))
        "a diff of the engine itself renders through RENDER-UNIFIED-CONTENT")
    (ok (equal "" (d:render-diff (d:compute-diff *note* (nl "a") (nl "a")) :unified nil)))
    (ok (signals (d:render-diff (d:compute-diff (d:find-diff-engine :sexp) '(:a 1) '(:a 2)) :unified nil)
            'd:unsupported-content)
        "an engine without a method still has no unified form")))

(deftest-parametrize extension-tree-unified
    ((level) :ids ("changes" "content" "summary")
     (:changes) (:content) (:summary))
  (flet ((render (type a b)
           (d:render-diff (d:compute-diff *note-tree* (apply #'note-tree type a) (apply #'note-tree type b)
                                          :level level)
                          :unified nil)))
    (let ((modify (render :note `(("n.note" ,(nl "x" "a"))) `(("n.note" ,(nl "x" "b")))))
          (add (render :note '() `(("n.note" ,(nl "x")))))
          (remove (render :note `(("n.note" ,(nl "x"))) '())))
      (ok (null (search "Binary" modify)) "a :note file is not binary")
      (ok (equal (nl "diff --git a/n.note b/n.note" "--- a/n.note" "+++ b/n.note"
                     "@@ -2 +2 @@" "-a" "+b")
                 modify))
      (ok (equal (render :text '() `(("n.note" ,(nl "x")))) add) "add renders like a text file")
      (ok (equal (render :text `(("n.note" ,(nl "x"))) '()) remove) "remove renders like a text file"))))

(deftest extension-tree-unified-fallback
  (let ((out (d:render-diff (d:compute-diff (make-instance 'd:tree-diff-engine)
                                            (d:make-tree "c" (d:make-tree-file '(:a 1) :type :sexp))
                                            (d:make-tree "c" (d:make-tree-file '(:a 2) :type :sexp)))
                            :unified nil)))
    (ok (equal (nl "diff --git a/c b/c" "Binary files a/c and b/c differ") out)
        "an engine without RENDER-UNIFIED-CONTENT stays binary")))

(deftest extension-old-names
  ;; The 0.2 internal names stay the same functions (engines written against 0.2).
  (ok (eq #'d:apply-change (fdefinition 'diff-protocol::%apply-change)))
  (ok (eq #'d:put-value (fdefinition 'diff-protocol::%put-value)))
  (ok (eq #'d:merge-states (fdefinition 'diff-protocol::%three-way)))
  (ok (eq #'d:rebase-states (fdefinition 'diff-protocol::%rebase)))
  (ok (eq #'d:invert-change (fdefinition 'diff-protocol::%invert-change)))
  (ok (eq *note* (diff-protocol::%register-engine :note (make-instance 'note-engine)))))
