(in-package #:diff-protocol/tests)

;;; R1 text engine: lines, hunks, footprints, levels, context, algorithms, apply.
;;; A hunk is a CHANGE: path (P), P the 0-based line index in the state the change
;;; applies to; before / after are lists of lines (with terminators).

(defun tdiff (a b &rest keys &key (engine *text*) &allow-other-keys)
  (let ((keys (loop for (k v) on keys by #'cddr unless (eq k :engine) nconc (list k v))))
    (apply #'d:compute-diff engine a b keys)))

(defun hunk-shape (c)
  "(kind path before after footprint) of one change, for EQUAL comparison."
  (list (d:change-kind c) (d:change-path c) (d:change-before c) (d:change-after c)
        (d:change-footprint c)))

(defun shapes (diff) (mapcar #'hunk-shape (d:diff-changes diff)))

(deftest-parametrize split-lines-cases
    ((string expected)
     :ids ("empty" "no-eol" "one" "two-no-eol" "crlf" "blank-lines" "cr-alone")
     ("" '())
     ("a" '("a"))
     ((nl "a") (list (nl "a")))
     ((cat (nl "a") "b") (list (nl "a") "b"))
     ((crlf "a" "b") (list (crlf "a") (crlf "b")))
     ((nl "" "") (list (nl "") (nl "")))
     ((cat "a" (string #\Return) "b") (list (cat "a" (string #\Return) "b"))))
  (let ((lines (d:split-lines string)))
    (ok (equal expected lines))
    (ok (string= string (apply #'cat lines)) "concatenation restores the string")))

(deftest text-engine-defaults
  (ok (eq :text (d:engine-name *text*)))
  (ok (typep (d:find-diff-engine :text) 'd:text-diff-engine) "registered under :text")
  (ok (eq :myers (d:text-engine-algorithm (d:find-diff-engine :text))))
  (ok (eq :myers (d:text-engine-algorithm *text*)))
  (ok (eq :histogram (d:text-engine-algorithm *histogram*)))
  (ok (eq :text (d:engine-name *histogram*)))
  (ok (signals (make-instance 'd:text-diff-engine :algorithm :bogus) 'type-error)))

(deftest-parametrize text-unsupported-content
    ((a b)
     :ids ("number" "list" "nil-vs-string" "octets")
     (1 "a")
     ("a" '("a"))
     (nil "a")
     ("a" (octets 97)))
  (ok (signals (tdiff a b) 'd:unsupported-content)))

(deftest text-state-equal
  (ok (tx= "" ""))
  (ok (tx= (nl "a") (nl "a")))
  (ok (not (tx= (nl "a") "a")) "trailing newline matters")
  (ok (not (tx= (crlf "a") (nl "a"))) "CR matters")
  (ok (not (tx= "a" "A"))))

(deftest-parametrize text-hunks
    ((a b expected)
     :ids ("identical" "modify" "insert" "delete" "two-hunks-descending" "add-eol" "crlf-to-lf"
           "all-different" "from-empty" "to-empty" "adjacent-runs-one-hunk")
     ((nl "a" "b") (nl "a" "b") '())
     ((nl "a" "b" "c") (nl "a" "B" "c")
      (list (list :modify '(1) (list (nl "b")) (list (nl "B")) (list (text-fp 1 2)))))
     ((nl "a" "c") (nl "a" "b" "c")
      (list (list :add '(1) nil (list (nl "b")) (list (text-fp 1 1)))))
     ((nl "a" "b" "c") (nl "a" "c")
      (list (list :remove '(1) (list (nl "b")) nil (list (text-fp 1 2)))))
     ((nl "a" "b" "c" "d" "e") (nl "A" "b" "c" "d" "E")
      (list (list :modify '(4) (list (nl "e")) (list (nl "E")) (list (text-fp 4 5)))
            (list :modify '(0) (list (nl "a")) (list (nl "A")) (list (text-fp 0 1)))))
     ((cat (nl "a") "b") (nl "a" "b")
      (list (list :modify '(1) (list "b") (list (nl "b")) (list (text-fp 1 2)))))
     ((crlf "a" "b") (cat (nl "a") (crlf "b"))
      (list (list :modify '(0) (list (crlf "a")) (list (nl "a")) (list (text-fp 0 1)))))
     ((nl "a" "b") (nl "x" "y")
      (list (list :modify '(0) (list (nl "a") (nl "b")) (list (nl "x") (nl "y")) (list (text-fp 0 2)))))
     ("" (cat (nl "a") "b")
      (list (list :add '(0) nil (list (nl "a") "b") (list (text-fp 0 0)))))
     ((nl "a" "b") ""
      (list (list :remove '(0) (list (nl "a") (nl "b")) nil (list (text-fp 0 2)))))
     ((nl "k" "a" "b" "k") (nl "k" "x" "k")
      (list (list :modify '(1) (list (nl "a") (nl "b")) (list (nl "x")) (list (text-fp 1 3))))))
  (dolist (engine (list *text* *histogram*))
    (let ((diff (tdiff a b :engine engine)))
      (ok (typep diff 'd:diff))
      (ok (eq :changes (d:diff-level diff)))
      (ok (typep (d:diff-engine diff) 'd:text-diff-engine))
      (ok (equal expected (shapes diff)) (format nil "~(~A~) hunks" (d:text-engine-algorithm engine))))))

(defun hunk-invariants-p (a diff)
  "Minimal hunks of DIFF against A at level :CHANGES: strictly descending, separated by
at least one unchanged line, BEFORE = the lines of A at the path, kind by emptiness,
footprint \"text:/P-(P+|before|)\"."
  (let ((lines (coerce (lines-of a) 'vector))
        (limit nil))
    (dolist (c (d:diff-changes diff) t)
      (let* ((p (first (d:change-path c)))
             (before (d:change-before c))
             (after (d:change-after c))
             (end (+ p (length before))))
        (unless (and (= 1 (length (d:change-path c)))
                     (or (null limit) (< end limit))
                     (<= end (length lines))
                     (equal before (coerce (subseq lines p end) 'list))
                     (or before after)
                     (eq (d:change-kind c)
                         (cond ((null before) :add) ((null after) :remove) (t :modify)))
                     (equal (list (text-fp p end)) (d:change-footprint c)))
          (return nil))
        (setf limit p)))))

(deftest-parametrize text-hunk-invariants
    ((engine a b level) :ids (text-pair-ids :levels '(:changes))
                        :rows (text-pair-rows :levels '(:changes)))
  (ok (hunk-invariants-p a (d:compute-diff engine a b :level level))))

(deftest-parametrize myers-is-minimal
    ((engine a b level) :ids (text-pair-ids :engines '(*text*) :levels '(:changes))
                        :rows (text-pair-rows :engines '(*text*) :levels '(:changes)))
  (declare (ignorable level))
  (let* ((diff (d:compute-diff engine a b))
         (changed (loop for c in (d:diff-changes diff)
                        sum (+ (length (d:change-before c)) (length (d:change-after c)))))
         (la (length (lines-of a)))
         (lb (length (lines-of b))))
    (ok (= changed (- (+ la lb) (* 2 (lcs-length (lines-of a) (lines-of b)))))
        "removed + added lines = |a| + |b| - 2 LCS")))

(deftest histogram-anchors-on-rare-lines
  ;; "foo" is the only unique common line; histogram anchors on it and extends the
  ;; match both ways, so the new block is inserted at the top.
  (let ((a (nl "{" "foo" "}"))
        (b (nl "{" "bar" "}" "{" "foo" "}")))
    (ok (equal (list (list :add '(0) nil (list (nl "{") (nl "bar") (nl "}")) (list (text-fp 0 0))))
               (shapes (tdiff a b :engine *histogram*))))
    (ok (tx= b (d:apply-diff a (tdiff a b :engine *histogram*))))))

(deftest text-levels
  (let ((a (nl "a" "b" "c" "d" "e"))
        (b (nl "A" "b" "c" "d" "E")))
    (let* ((summary (tdiff a b :level :summary))
           (root (first (d:diff-changes summary))))
      (ok (eq :summary (d:diff-level summary)))
      (ok (= 1 (length (d:diff-changes summary))) "one root change for the whole text")
      (ok (eq :modify (d:change-kind root)))
      (ok (null (d:change-path root)))
      (ok (equal '("text:/") (d:change-footprint root)))
      (ok (null (d:change-children root)))
      (ok (not (d:change-before-loaded-p root)) "lazy, not forced by compute-diff")
      (ok (not (d:change-after-loaded-p root)))
      (ok (equal a (d:change-before root)))
      (ok (equal b (d:change-after root))))
    (let* ((content (tdiff a b :level :content))
           (root (first (d:diff-changes content))))
      (ok (= 1 (length (d:diff-changes content))))
      (ok (null (d:change-path root)))
      (ok (equal a (d:change-before root)))
      (ok (equal '((4) (0)) (mapcar #'d:change-path (d:change-children root))) "children = hunks")
      (ok (equal '("text:/" "text:/0-1" "text:/4-5") (d:diff-footprint content))))
    (ok (equal '((4) (0)) (paths (tdiff a b :level :changes))))
    (dolist (level *levels*)
      (ok (d:diff-empty-p (tdiff a a :level level))))))

(deftest-parametrize text-context
    ((a b context expected)
     :ids ("one-line-context" "merged-hunks" "separate-hunks" "edge-truncated" "zero-is-default")
     ((nl "1" "2" "3" "4" "5" "6" "7" "8" "9") (nl "1" "2" "3" "4" "X" "6" "7" "8" "9") 1
      (list (list :modify '(3) (list (nl "4") (nl "5") (nl "6")) (list (nl "4") (nl "X") (nl "6"))
                  (list (text-fp 3 6)))))
     ;; gap of 1 unchanged line <= 2 x context → one hunk
     ((nl "1" "2" "3" "4" "5" "6") (nl "1" "B" "3" "D" "5" "6") 1
      (list (list :modify '(0) (list (nl "1") (nl "2") (nl "3") (nl "4") (nl "5"))
                  (list (nl "1") (nl "B") (nl "3") (nl "D") (nl "5"))
                  (list (text-fp 0 5)))))
     ;; gap of 5 > 2 → two hunks, descending
     ((nl "1" "2" "3" "4" "5" "6" "7" "8" "9") (nl "1" "B" "3" "4" "5" "6" "7" "H" "9") 1
      (list (list :modify '(6) (list (nl "7") (nl "8") (nl "9")) (list (nl "7") (nl "H") (nl "9"))
                  (list (text-fp 6 9)))
            (list :modify '(0) (list (nl "1") (nl "2") (nl "3")) (list (nl "1") (nl "B") (nl "3"))
                  (list (text-fp 0 3)))))
     ((nl "1" "2" "3") (nl "X" "2" "3") 3
      (list (list :modify '(0) (list (nl "1") (nl "2") (nl "3")) (list (nl "X") (nl "2") (nl "3"))
                  (list (text-fp 0 3)))))
     ((nl "1" "2" "3") (nl "1" "X" "3") 0
      (list (list :modify '(1) (list (nl "2")) (list (nl "X")) (list (text-fp 1 2))))))
  (let ((diff (tdiff a b :context context)))
    (ok (equal expected (shapes diff)))
    (ok (tx= b (d:apply-diff a diff)) "context hunks apply")
    (ok (tx= a (d:apply-diff b (d:invert-diff diff))))))

(deftest text-invert-footprints
  ;; Inverse hunks: same path, before/after swapped, footprint over the new before.
  (let* ((a (nl "a" "b" "c" "d" "e"))
         (b (nl "a" "B" "C" "c" "d"))
         (diff (tdiff a b))
         (inv (d:invert-diff diff)))
    (ok (equal (list (list :remove '(4) (list (nl "e")) nil (list (text-fp 4 5)))
                     (list :modify '(1) (list (nl "b")) (list (nl "B") (nl "C")) (list (text-fp 1 2))))
               (shapes diff)))
    (ok (equal (list (list :modify '(1) (list (nl "B") (nl "C")) (list (nl "b")) (list (text-fp 1 3)))
                     (list :add '(4) nil (list (nl "e")) (list (text-fp 4 4))))
               (shapes inv))
        "order reversed, kinds swapped, footprints recomputed")
    (ok (tx= a (d:apply-diff b inv)))
    (ok (equal (shapes diff) (shapes (d:invert-diff inv))) "double inverse restores footprints")))

(deftest text-invert-summary-stays-lazy
  (let ((inv (d:invert-diff (tdiff (nl "a") (nl "b") :level :summary))))
    (ok (notany #'d:change-before-loaded-p (d:diff-changes inv)))
    (ok (equal '("text:/") (d:change-footprint (first (d:diff-changes inv)))))
    (ok (tx= (nl "a") (d:apply-diff (nl "b") inv)))))

(deftest text-apply-conflicts
  (let ((diff (tdiff (nl "a" "b" "c") (nl "a" "B" "c"))))
    (let ((c (handler-case (progn (d:apply-diff (nl "a" "x" "c") diff) nil)
               (d:apply-conflict (e) e))))
      (ok (typep c 'd:apply-conflict))
      (ok (eq :before-mismatch (and c (d:apply-conflict-reason c))))
      (ok (equal (list (nl "x")) (and c (d:apply-conflict-actual c)))
          "actual = the lines found at the hunk's range"))
    (let ((c (handler-case (progn (d:apply-diff (nl "a") diff) nil)
               (d:apply-conflict (e) e))))
      (ok (eq :missing (and c (d:apply-conflict-reason c))) "range past the end"))
    (multiple-value-bind (state skipped)
        (handler-bind ((d:apply-conflict #'d:skip-change))
          (d:apply-diff (nl "a" "x" "c") diff))
      (ok (equal (nl "a" "x" "c") state))
      (ok (= 1 (length skipped))))
    (ok (equal (nl "a" "Q" "c")
               (handler-bind ((d:apply-conflict (lambda (c) (declare (ignore c))
                                                   (use-value (list (nl "Q"))))))
                 (d:apply-diff (nl "a" "x" "c") diff)))
        "use-value: lines put in place of the hunk's range")
    (ok (signals (d:apply-diff 42 diff) 'd:unsupported-content))))

(deftest text-sexp-round-trip
  (let* ((a (cat (crlf "a") (nl "b") "c"))
         (b (cat (nl "a" "B") "c" ))
         (diff (tdiff a b))
         (text (d:render-diff diff :sexp nil))
         (parsed (d:parse-diff :sexp text)))
    (ok (typep (d:diff-engine parsed) 'd:text-diff-engine) "engine :text via find-diff-engine")
    (ok (equal text (d:render-diff parsed :sexp nil)))
    (ok (tx= b (d:apply-diff a parsed)))))
