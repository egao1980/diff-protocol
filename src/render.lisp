(in-package #:diff-protocol)

;;; render-diff :summary / :sexp and parse-diff :sexp (brief: "Render / parse").

(defun %call-with-output (stream fn)
  "STREAM NIL → string; T → *STANDARD-OUTPUT* (dynamic); otherwise STREAM. FN takes a stream."
  (cond ((null stream) (with-output-to-string (s) (funcall fn s)))
        ((eq stream t) (funcall fn *standard-output*) nil)
        (t (funcall fn stream) nil)))

(defun %kind-letter (kind)
  (ecase kind (:add "A") (:remove "D") (:modify "M") (:move "R")))

(defmethod render-diff ((d diff) (format (eql :summary)) stream)
  (%call-with-output stream
                     (lambda (s)
                       (dolist (c (diff-changes d))
                         (cl:format s "~A~{ ~A~}~%"
                                    (%kind-letter (change-kind c)) (change-footprint c))))))

(defgeneric %change-form (change)
  (:documentation "Plain-data :SEXP form of one change (forces lazy content)."))

(defmethod %change-form ((c change))
  (let ((k (change-kind c)))
    `(:change :kind ,k :path ,(change-path c)
              ,@(when (eq k :move) `(:from ,(change-from c)))
              :footprint ,(change-footprint c)
              ,@(unless (eq k :add) `(:before ,(change-before c)))
              ,@(unless (eq k :remove) `(:after ,(change-after c)))
              ,@(when (change-children c)
                  `(:children ,(mapcar #'%change-form (change-children c)))))))

(defmethod %change-form ((c opaque-change))
  `(:opaque :kind ,(change-kind c) :footprint ,(change-footprint c)
            :description ,(opaque-change-description c)))

(defmethod render-diff ((d diff) (format (eql :sexp)) stream)
  (let ((form `(:diff :version 1
                      :engine ,(and (diff-engine d) (engine-name (diff-engine d)))
                      :level ,(diff-level d)
                      :changes ,(mapcar #'%change-form (diff-changes d)))))
    (%call-with-output stream
                       (lambda (s)
                         (with-standard-io-syntax
                           (let ((*package* (find-package '#:keyword))
                                 (*print-pretty* nil)
                                 ;; SBCL prints base-strings readably as #A(...); plain "..." is the
                                 ;; canonical form, and the content is plain data (validated by the engine).
                                 (*print-readably* nil))
                             (prin1 form s)))))))

(defun %change-from-form (form)
  (unless (and (consp form) (member (first form) '(:change :opaque)))
    (error 'type-error :datum form :expected-type '(cons (member :change :opaque))))
  (ecase (first form)
    (:change
     (destructuring-bind (&key kind path from footprint (before nil before-p)
                            (after nil after-p) children)
         (rest form)
       (apply #'make-change kind :path path :from from :footprint footprint
              :children (mapcar #'%change-from-form children)
              (append (when before-p (list :before before))
                      (when after-p (list :after after))))))
    (:opaque
     (destructuring-bind (&key kind footprint description) (rest form)
       (make-opaque-change kind :footprint footprint :description description)))))

(defmethod parse-diff ((format (eql :sexp)) input)
  (let ((form (with-standard-io-syntax
                ;; WITH-STANDARD-IO-SYNTAX binds *READ-EVAL* to T: turn it off inside.
                ;; *PACKAGE* stays CL-USER so COMMON-LISP:NIL and keywords read back.
                (let ((*read-eval* nil))
                  (etypecase input
                    (string (read-from-string input))
                    (stream (read input)))))))
    (unless (and (consp form) (eq (first form) :diff))
      (error 'type-error :datum form :expected-type '(cons (eql :diff))))
    (destructuring-bind (&key version engine (level :changes) changes) (rest form)
      (unless (eql version 1)
        (error 'type-error :datum version :expected-type '(eql 1)))
      (make-diff (mapcar #'%change-from-form changes)
                 :engine (and engine (find-diff-engine engine))
                 :level level))))
