(in-package #:diff-protocol/tests)

;;; Fixed corpus for the law suite (brief: "Laws").
;;; DEFTEST-PARAMETRIZE splices each row value into a LET as a form, so it is
;;; evaluated: rows carry quoted states (Q), never bare lists.

(defvar *engine* (make-instance 'd:sexp-diff-engine))

(defparameter *levels* '(:summary :changes :content))

(defparameter *states*
  '((:empty nil)
    (:flat (:a 1 :b "two" :c :three))
    (:flat-mod (:a 1 :b "TWO" :c :three))
    (:flat-add (:a 1 :b "two" :c :three :d 4))
    (:flat-del (:a 1 :c :three))
    (:flat-reorder (:c :three :a 1 :b "two"))
    (:renamed (:a 1 :bb "two" :c :three))
    (:nested (:name "w" :cfg (:x 1 :y (:p 1 :q 2)) :tags ("a" "b")))
    (:nested-deep (:name "w" :cfg (:x 1 :y (:p 1 :q 3)) :tags ("a" "b" "c")))
    (:nested-short (:name "w" :cfg (:x 2) :tags ("a")))
    (:list (1 2 3 4))
    (:list-mixed (1 (:k 1) "s" (2 3)))
    (:atom 42)
    (:string "hello")))

(defun state (name)
  (let ((entry (assoc name *states*)))
    (assert entry () "No corpus state ~S" name)
    (second entry)))

(defun q (x) (list 'quote x))

(defun st= (a b) (d:state-equal *engine* a b))

(defun diff-of (a b &optional (level :changes))
  (d:compute-diff *engine* a b :level level))

(defparameter *pairs*
  '((:empty :flat) (:flat :empty) (:flat :flat-mod) (:flat :flat-add) (:flat :flat-del)
    (:flat :renamed) (:flat-reorder :flat-mod) (:nested :nested-deep) (:nested-deep :nested)
    (:nested :nested-short) (:nested-short :nested) (:list :list-mixed) (:list-mixed :list)
    (:flat :list) (:list :atom) (:atom :string) (:string :flat) (:nested :flat)))

(defparameter *triples*
  '((:flat :flat-mod :flat-add) (:empty :nested :nested-deep) (:list :list-mixed :atom)
    (:nested :nested-short :nested-deep) (:flat :renamed :flat-del) (:atom :flat :empty)))

(defun pair-level-rows ()
  "Rows (a b level) as forms, one per corpus pair and level."
  (loop for (x y) in *pairs*
        nconc (loop for level in *levels*
                    collect (list (q (state x)) (q (state y)) level))))

(defun pair-level-ids ()
  (loop for (x y) in *pairs*
        nconc (loop for level in *levels*
                    collect (format nil "~(~A>~A@~A~)" x y level))))

(defun state-level-rows ()
  (loop for (nil s) in *states*
        nconc (loop for level in *levels* collect (list (q s) level))))

(defun state-level-ids ()
  (loop for (name) in *states*
        nconc (loop for level in *levels* collect (format nil "~(~A@~A~)" name level))))

(defun triple-rows ()
  (loop for (x y z) in *triples* collect (list (q (state x)) (q (state y)) (q (state z)))))

(defun triple-ids ()
  (loop for (x y z) in *triples* collect (format nil "~(~A>~A>~A~)" x y z)))

(defun sorted-footprints (conflicts)
  "Conflict footprints as a sorted list of lists, for order-free comparison."
  (sort (mapcar (lambda (c) (copy-list (d:conflict-footprint c))) conflicts)
        (lambda (x y) (string< (format nil "~{~A~^ ~}" x) (format nil "~{~A~^ ~}" y)))))

(defun kinds (diff) (mapcar #'d:change-kind (d:diff-changes diff)))
(defun paths (diff) (mapcar #'d:change-path (d:diff-changes diff)))
