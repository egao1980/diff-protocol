(in-package #:diff-protocol/tests)

;;; R1 diff3: THREE-WAY-DIFF on the text engine, MERGE-TEXTS, text REBASE-DIFF.
;;; Two hunks (base ranges [s1,e1), [s2,e2)) interact when no unchanged base line
;;; separates them: s1 <= e2 and s2 <= e1. Interacting hunks form one region.
;;; Row: base ours theirs merged conflict-footprints.

(defparameter *diff3-cases*
  (list
   (list "disjoint" (nl "1" "2" "3" "4" "5") (nl "A" "2" "3" "4" "5") (nl "1" "2" "3" "4" "E")
         (nl "A" "2" "3" "4" "E") '())
   (list "same-edit" (nl "a" "b" "c") (nl "a" "B" "c") (nl "a" "B" "c")
         (nl "a" "B" "c") '())
   (list "same-line" (nl "a" "b" "c") (nl "a" "B" "c") (nl "a" "X" "c")
         (nl "a" "b" "c") (list (list (text-fp 1 2))))
   (list "adjacent-lines" (nl "a" "b" "c" "d") (nl "a" "B" "c" "d") (nl "a" "b" "C" "d")
         (nl "a" "b" "c" "d") (list (list (text-fp 1 3))))
   (list "insert-same-point" (nl "a" "b") (nl "a" "X" "b") (nl "a" "Y" "b")
         (nl "a" "b") (list (list (text-fp 1 1))))
   (list "insert-touching-edit" (nl "a" "b" "c") (nl "a" "X" "b" "c") (nl "a" "B" "c")
         (nl "a" "b" "c") (list (list (text-fp 1 2))))
   (list "delete-vs-edit" (nl "a" "b" "c") (nl "a" "c") (nl "a" "B" "c")
         (nl "a" "b" "c") (list (list (text-fp 1 2))))
   (list "delete-all-one-side" (nl "a" "b") "" (nl "a" "b")
         "" '())
   (list "append-and-edit-first" (nl "a" "b" "c") (nl "a" "b" "c" "d") (nl "A" "b" "c")
         (nl "A" "b" "c" "d") '())
   (list "no-eol" (cat (nl "a" "m") "b") (nl "a" "m" "b") (cat (nl "A" "m") "b")
         (nl "A" "m" "b") '())
   (list "crlf" (crlf "a" "b" "c" "d" "e") (crlf "A" "b" "c" "d" "e") (crlf "a" "b" "c" "d" "E")
         (crlf "A" "b" "c" "d" "E") '())
   (list "two-regions" (nl "1" "2" "3" "4" "5" "6" "7") (nl "1" "X" "3" "4" "5" "6" "Z")
         (nl "1" "Y" "3" "4" "W" "6" "7")
         (nl "1" "2" "3" "4" "W" "6" "Z") (list (list (text-fp 1 2))))))

(defun diff3-rows ()
  (loop for (nil base ours theirs merged conflicts) in *diff3-cases*
        collect (list base ours theirs merged (q conflicts))))

(defun diff3-ids () (mapcar #'first *diff3-cases*))

(deftest-parametrize diff3
    ((base ours theirs merged conflicts) :ids (diff3-ids) :rows (diff3-rows))
  (dolist (engine (list *text* *histogram*))
    (multiple-value-bind (diff found) (d:three-way-diff engine base ours theirs)
      (ok (typep diff 'd:diff))
      (ok (eq :changes (d:diff-level diff)))
      (ok (every (lambda (c) (typep c 'd:diff-conflict)) found) "conflicts are data")
      (ok (tx= merged (d:apply-diff base diff)) "conflict regions keep base")
      (ok (equal conflicts (sorted-footprints found)))
      (ok (hunk-invariants-p base diff) "merged hunks: descending, base coordinates"))))

(deftest-parametrize diff3-symmetric
    ((base ours theirs merged conflicts) :ids (diff3-ids) :rows (diff3-rows))
  (multiple-value-bind (d1 c1) (d:three-way-diff *text* base ours theirs)
    (multiple-value-bind (d2 c2) (d:three-way-diff *text* base theirs ours)
      (ok (tx= merged (d:apply-diff base d2)))
      (ok (tx= (d:apply-diff base d1) (d:apply-diff base d2)))
      (ok (equal conflicts (sorted-footprints c2)))
      (ok (equal (sorted-footprints c1) (sorted-footprints c2))))))

(deftest diff3-conflict-sides
  (let ((c (first (nth-value 1 (d:three-way-diff *text* (nl "a" "b" "c") (nl "a" "B" "c")
                                                 (nl "a" "X" "c"))))))
    (ok (typep c 'd:diff-conflict))
    (ok (equal (list :modify '(1) (list (nl "b")) (list (nl "B")) (list (text-fp 1 2)))
               (hunk-shape (d:conflict-ours c))))
    (ok (equal (list :modify '(1) (list (nl "b")) (list (nl "X")) (list (text-fp 1 2)))
               (hunk-shape (d:conflict-theirs c))))
    (ok (equal (list (text-fp 1 2)) (d:conflict-footprint c))))
  (let ((c (first (nth-value 1 (d:three-way-diff *text* (nl "a" "b" "c" "d") (nl "a" "B" "c" "d")
                                                 (nl "a" "b" "C" "d"))))))
    (ok (equal (list (nl "b") (nl "c")) (d:change-before (d:conflict-ours c))) "region = union")
    (ok (equal (list (nl "B") (nl "c")) (d:change-after (d:conflict-ours c))))
    (ok (equal (list (nl "b") (nl "C")) (d:change-after (d:conflict-theirs c)))))
  (let ((c (first (nth-value 1 (d:three-way-diff *text* (nl "a" "b" "c") (nl "a" "c")
                                                 (nl "a" "B" "c"))))))
    (ok (eq :remove (d:change-kind (d:conflict-ours c))))
    (ok (null (d:change-after (d:conflict-ours c))))
    (ok (eq :modify (d:change-kind (d:conflict-theirs c))))))

(deftest-parametrize merge-texts-cases
    ((base ours theirs keys expected nconflicts)
     :ids ("clean" "conflict" "diff3-style" "labels" "no-eol-sides" "empty-side")
     ((nl "1" "2" "3" "4" "5") (nl "A" "2" "3" "4" "5") (nl "1" "2" "3" "4" "E") '()
      (nl "A" "2" "3" "4" "E") 0)
     ((nl "a" "b" "c") (nl "a" "B" "c") (nl "a" "X" "c") '()
      (nl "a" "<<<<<<< ours" "B" "=======" "X" ">>>>>>> theirs" "c") 1)
     ((nl "a" "b" "c") (nl "a" "B" "c") (nl "a" "X" "c") '(:style :diff3)
      (nl "a" "<<<<<<< ours" "B" "||||||| base" "b" "=======" "X" ">>>>>>> theirs" "c") 1)
     ((nl "a" "b" "c") (nl "a" "B" "c") (nl "a" "X" "c")
      '(:style :diff3 :ours-label "HEAD" :base-label "merged common ancestors" :theirs-label "topic")
      (nl "a" "<<<<<<< HEAD" "B" "||||||| merged common ancestors" "b" "=======" "X"
          ">>>>>>> topic" "c") 1)
     ;; A side whose last line has no newline gets one before the next marker.
     ((cat (nl "a") "b") (cat (nl "a") "B") (cat (nl "a") "X") '()
      (nl "a" "<<<<<<< ours" "B" "=======" "X" ">>>>>>> theirs") 1)
     ((nl "a" "b" "c") (nl "a" "c") (nl "a" "B" "c") '()
      (nl "a" "<<<<<<< ours" "=======" "B" ">>>>>>> theirs" "c") 1))
  (multiple-value-bind (text conflicts) (apply #'d:merge-texts base ours theirs keys)
    (ok (equal expected text))
    (ok (= nconflicts (length conflicts)))
    (ok (every (lambda (c) (typep c 'd:diff-conflict)) conflicts))))

(deftest merge-texts-engine
  (ok (equal (nl "A" "2" "3" "4" "E")
             (d:merge-texts (nl "1" "2" "3" "4" "5") (nl "A" "2" "3" "4" "5") (nl "1" "2" "3" "4" "E")
                            :engine *histogram*))))

(deftest text-rebase
  ;; Rebase D (edit last line) onto ONTO (insert at top, edit line 2), both against BASE.
  (let* ((base (nl "a" "b" "c" "d" "e"))
         (onto-state (nl "top" "a" "b" "C" "d" "e"))
         (d-state (nl "a" "b" "c" "d" "E"))
         (onto (d:compute-diff *text* base onto-state)))
    (multiple-value-bind (rebased conflicts) (d:rebase-diff (d:compute-diff *text* base d-state) onto base)
      (ok (typep rebased 'd:diff))
      (ok (null conflicts))
      (ok (tx= (nl "top" "a" "b" "C" "d" "E") (d:apply-diff onto-state rebased))
          "rebased diff applies after ONTO")))
  (let* ((base (nl "a" "b" "c"))
         (onto-state (nl "a" "X" "c")))
    (multiple-value-bind (rebased conflicts)
        (d:rebase-diff (d:compute-diff *text* base (nl "a" "B" "c"))
                       (d:compute-diff *text* base onto-state) base)
      (ok (= 1 (length conflicts)))
      (ok (equal (list (nl "B")) (d:change-after (d:conflict-ours (first conflicts)))) "ours = from D")
      (ok (equal (list (nl "X")) (d:change-after (d:conflict-theirs (first conflicts)))) "theirs = from ONTO")
      (ok (tx= onto-state (d:apply-diff onto-state rebased)) "conflict regions keep ONTO's text"))))
