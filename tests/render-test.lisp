(in-package #:diff-protocol/tests)

;;; render-diff :summary / :sexp shape, parse-diff :sexp round trip.

(defun read-one (string)
  "Read exactly one form from STRING without #. evaluation."
  (let ((*read-eval* nil))
    (multiple-value-bind (form end) (read-from-string string)
      (assert (eq :eof (read-from-string string nil :eof :start end)) ()
              "More than one form in ~S" string)
      form)))

(defparameter *render-a* '(:a 1 :b 2 :c 3))
(defparameter *render-b* '(:a 9 :z 2 :d 4))

(deftest render-summary-lines
  ;; modify :a, move :b→:z, remove :c, add :d
  (let ((diff (diff-of *render-a* *render-b*)))
    (ok (equal (format nil "M sexp:/a~%R sexp:/b sexp:/z~%D sexp:/c~%A sexp:/d~%")
               (d:render-diff diff :summary nil)))))

(deftest render-summary-streams
  (let* ((diff (diff-of *render-a* *render-b*))
         (expected (d:render-diff diff :summary nil)))
    (ok (stringp expected))
    (ok (equal expected (with-output-to-string (s) (d:render-diff diff :summary s))))
    (ok (equal expected (with-output-to-string (*standard-output*)
                          (d:render-diff diff :summary t))))))

(deftest render-summary-empty
  (ok (equal "" (d:render-diff (diff-of '(:a 1) '(:a 1)) :summary nil))))

(deftest render-summary-content-level-roots
  (ok (equal (format nil "M sexp:/cfg~%M sexp:/name~%A sexp:/new~%")
             (d:render-diff (diff-of '(:cfg (:x 1) :name "w") '(:cfg (:x 2) :name "v" :new 1) :content)
                            :summary nil))
      "one line per element of diff-changes (roots at :content)"))

(deftest render-sexp-shape
  (ok (equal '(:diff :version 1 :engine :sexp :level :changes
               :changes ((:change :kind :modify :path (:a) :footprint ("sexp:/a")
                          :before 1 :after 2)))
             (read-one (d:render-diff (diff-of '(:a 1) '(:a 2)) :sexp nil)))))

(deftest render-sexp-kinds
  (let ((form (read-one (d:render-diff (diff-of *render-a* *render-b*) :sexp nil))))
    (ok (equal '((:change :kind :modify :path (:a) :footprint ("sexp:/a") :before 1 :after 9)
                 (:change :kind :move :path (:z) :from (:b) :footprint ("sexp:/b" "sexp:/z")
                  :before 2 :after 2)
                 (:change :kind :remove :path (:c) :footprint ("sexp:/c") :before 3)
                 (:change :kind :add :path (:d) :footprint ("sexp:/d") :after 4))
               (getf (rest form) :changes))
        ":before omitted on :add, :after on :remove, :from only on :move")))

(deftest render-sexp-children
  (let ((form (read-one (d:render-diff (diff-of '(:cfg (:x 1)) '(:cfg (:x 2)) :content) :sexp nil))))
    (ok (eq :content (getf (rest form) :level)))
    (ok (equal '((:change :kind :modify :path (:cfg) :footprint ("sexp:/cfg")
                  :before (:x 1) :after (:x 2)
                  :children ((:change :kind :modify :path (:cfg :x) :footprint ("sexp:/cfg/x")
                              :before 1 :after 2))))
               (getf (rest form) :changes)))))

(deftest render-sexp-opaque
  (let ((diff (d:make-diff (list (d:make-opaque-change :modify :footprint '("table:widgets")
                                                               :description "backfill"))
                           :engine *engine*)))
    (ok (equal '((:opaque :kind :modify :footprint ("table:widgets") :description "backfill"))
               (getf (rest (read-one (d:render-diff diff :sexp nil))) :changes)))))

(deftest render-sexp-forces-lazy-content
  (let ((form (read-one (d:render-diff (diff-of '(:a 1) '(:a 2) :summary) :sexp nil))))
    (ok (equal '((:change :kind :modify :path (:a) :footprint ("sexp:/a") :before 1 :after 2))
               (getf (rest form) :changes)))))

(deftest-parametrize parse-sexp-round-trip
    ((a b level) :ids (pair-level-ids) :rows (pair-level-rows))
  (let* ((diff (diff-of a b level))
         (text (d:render-diff diff :sexp nil))
         (parsed (d:parse-diff :sexp text)))
    (ok (typep parsed 'd:diff))
    (ok (equal text (d:render-diff parsed :sexp nil)) "render → parse → render is string=")
    (ok (eq level (d:diff-level parsed)))
    (ok (eq :sexp (d:engine-name (d:diff-engine parsed))))
    (ok (equal (d:diff-footprint diff) (d:diff-footprint parsed)))
    (ok (st= b (d:apply-diff a parsed)))))

(deftest parse-sexp-from-stream
  (let* ((text (d:render-diff (diff-of *render-a* *render-b*) :sexp nil))
         (parsed (with-input-from-string (s text) (d:parse-diff :sexp s))))
    (ok (equal text (d:render-diff parsed :sexp nil)))))

(deftest parse-sexp-opaque
  (let* ((text (format nil "~S" '(:diff :version 1 :engine :sexp :level :changes
                                  :changes ((:opaque :kind :modify :footprint ("table:widgets")
                                             :description "backfill")))))
         (c (first (d:diff-changes (d:parse-diff :sexp text)))))
    (ok (typep c 'd:opaque-change))
    (ok (equal "backfill" (d:opaque-change-description c)))
    (ok (equal '("table:widgets") (d:change-footprint c)))))

(deftest parse-sexp-no-read-eval
  (ok (signals (d:parse-diff :sexp "#.(error \"read-eval must be off\")") 'reader-error)))
