(in-package #:diff-protocol/tests)

;;; R1 tree engine. Paths: file-level changes have a list of names; leaf changes
;;; add the leaf engine's steps (text: line index; sexp: keywords / indices);
;;; a mode change ends in D:TREE-MODE. Footprints: "tree:/<names>" for files,
;;; plus the leaf engine's string with the file path spliced in for leaves.

(defun tdf (a b &rest keys &key (engine *tree*) &allow-other-keys)
  (let ((keys (loop for (k v) on keys by #'cddr unless (eq k :engine) nconc (list k v))))
    (apply #'d:compute-diff engine (tr a) (tr b) keys)))

(defun tree-shape (c)
  "(kind path from footprint) of one change."
  (list (d:change-kind c) (d:change-path c) (d:change-from c) (d:change-footprint c)))

(defun tree-shapes (diff) (mapcar #'tree-shape (d:diff-changes diff)))

(deftest tree-engine-defaults
  (ok (eq :tree (d:engine-name *tree*)))
  (ok (typep (d:find-diff-engine :tree) 'd:tree-diff-engine) "registered under :tree")
  (ok (= 0.5 (d:tree-engine-rename-threshold *tree*)))
  (ok (eq t (d:tree-engine-detect-renames *tree*)))
  (ok (equal '((:text . :text) (:sexp . :sexp)) (d:tree-engine-leaf-engines *tree*))))

(deftest-parametrize tree-unsupported-content
    ((a b) :ids ("string" "plist" "file-node")
     ("x" '(:dir))
     ('(:a 1) '(:dir))
     ('(:dir) '(:file :type :text :content "x" :mode nil)))
  (ok (signals (d:compute-diff *tree* a b) 'd:unsupported-content)))

(deftest tree-add-remove
  (let ((diff (tdf '() `(("a" ,(nl "x"))))))
    (ok (eq :changes (d:diff-level diff)))
    (ok (typep (d:diff-engine diff) 'd:tree-diff-engine))
    (ok (equal '((:add ("a") nil ("tree:/a"))) (tree-shapes diff)))
    (let ((c (first (d:diff-changes diff))))
      (ok (d:tree-file-p (d:change-after c)) "after = the file node")
      (ok (equal (nl "x") (d:tree-file-content (d:change-after c))))
      (ok (null (d:change-before c)))))
  (let ((diff (tdf `(("d/e/f" ,(nl "x"))) '())))
    (ok (equal '((:remove ("d" "e" "f") nil ("tree:/d/e/f"))) (tree-shapes diff)))
    (ok (tr= (tr '()) (d:apply-diff (tr `(("d/e/f" ,(nl "x")))) diff))
        "emptied directories are pruned")))

(deftest tree-text-modify-levels
  (let ((a (tree-entries-of :nested))
        (b (tree-entries-of :nested-edit)))
    (ok (equal '((:add ("README" 1) nil ("tree:/README" "text:/README/1-1"))
                 (:modify ("src" "x.lisp" 1) nil ("tree:/src/x.lisp" "text:/src/x.lisp/1-2")))
               (tree-shapes (tdf a b)))
        ":changes = leaf hunks, file path + line index")
    (let ((hunk (second (d:diff-changes (tdf a b)))))
      (ok (equal (list (nl "(b)")) (d:change-before hunk)) "hunk sides are line lists")
      (ok (equal (list (nl "(B)")) (d:change-after hunk))))
    (let ((content (tdf a b :level :content)))
      (ok (equal '((:modify ("README") nil ("tree:/README"))
                   (:modify ("src" "x.lisp") nil ("tree:/src/x.lisp")))
                 (tree-shapes content)))
      (ok (equal '(("README" 1)) (mapcar #'d:change-path (d:change-children (first (d:diff-changes content)))))
          "children = the leaf hunks")
      (ok (d:tree-file-p (d:change-before (first (d:diff-changes content))))))
    (let ((summary (tdf a b :level :summary)))
      (ok (equal '((:modify ("README") nil ("tree:/README"))
                   (:modify ("src" "x.lisp") nil ("tree:/src/x.lisp")))
                 (tree-shapes summary)))
      (ok (notany #'d:change-children (d:diff-changes summary)))
      (ok (notany #'d:change-before-loaded-p (d:diff-changes summary)) "lazy")
      (ok (notany #'d:change-after-loaded-p (d:diff-changes summary))))
    (ok (equal '((:modify ("src" "x.lisp" 0) nil ("tree:/src/x.lisp" "text:/src/x.lisp/0-3")))
               (tree-shapes (tdf `(("src/x.lisp" ,(nl "(a)" "(b)" "(c)" "(d)")))
                                 `(("src/x.lisp" ,(nl "(a)" "(B)" "(c)" "(d)")))
                                 :context 1)))
        ":context is passed to text leaves")))

(deftest tree-leaf-dispatch
  (let ((diff (tdf (tree-entries-of :typed) (tree-entries-of :typed-edit))))
    (ok (equal '((:modify ("cfg" :a) nil ("tree:/cfg" "sexp:/cfg/a"))
                 (:add ("cfg" :b 2) nil ("tree:/cfg" "sexp:/cfg/b/2"))
                 (:modify ("logo") nil ("tree:/logo"))
                 (:modify ("t" 0) nil ("tree:/t" "text:/t/0-1")))
               (tree-shapes diff))
        "sexp leaves via the sexp engine, binary as a whole, text as hunks")
    (ok (equalp (octets 1 2 4) (d:tree-file-content (d:change-after (third (d:diff-changes diff))))))))

(deftest tree-type-change-is-whole
  (ok (equal '((:modify ("c") nil ("tree:/c")))
             (tree-shapes (tdf '(("c" "(:a 1)")) '(("c" (:a 1) :type :sexp))))))
  (let ((c (first (d:diff-changes (tdf '(("c" "(:a 1)")) '(("c" (:a 1) :type :sexp)))))))
    (ok (null (d:change-children c)))
    (ok (eq :sexp (d:tree-file-type (d:change-after c))))))

(deftest tree-mode-change
  (let ((a (tree-entries-of :nested))
        (b (tree-entries-of :mode)))
    (ok (equal (list (list :modify (list "src" "x.lisp" 'd:tree-mode) nil '("tree:/src/x.lisp")))
               (tree-shapes (tdf a b))))
    (let ((c (first (d:diff-changes (tdf a b)))))
      (ok (eql #o100644 (d:change-before c)) "NIL is read as #o100644")
      (ok (eql #o100755 (d:change-after c))))
    (let ((c (first (d:diff-changes (tdf a b :level :content)))))
      (ok (equal '("src" "x.lisp") (d:change-path c)))
      (ok (equal (list (list "src" "x.lisp" 'd:tree-mode)) (mapcar #'d:change-path (d:change-children c)))))
    (ok (d:diff-empty-p (tdf `(("a" "x")) `(("a" "x" :mode #o100644)))) "NIL ≡ #o100644")))

(deftest tree-exact-rename
  (let ((diff (tdf (tree-entries-of :nested) (tree-entries-of :renamed))))
    (ok (equal '((:move ("lib" "x.lisp") ("src" "x.lisp") ("tree:/src/x.lisp" "tree:/lib/x.lisp")))
               (tree-shapes diff)))
    (ok (tr= (d:change-before (first (d:diff-changes diff))) (d:change-after (first (d:diff-changes diff)))))))

(deftest tree-similar-rename
  (let* ((diff (tdf (tree-entries-of :nested) (tree-entries-of :renamed-edit)))
         (c (first (d:diff-changes diff))))
    (ok (equal '((:move ("lib" "x.lisp") ("src" "x.lisp") ("tree:/src/x.lisp" "tree:/lib/x.lisp")))
               (tree-shapes diff))
        "similarity 2*3/8 = 0.75 >= 0.5")
    (ok (equal (nl "(a)" "(b)" "(c)" "(d)") (d:tree-file-content (d:change-before c))))
    (ok (equal (nl "(a)" "(b)" "(c)" "(D)") (d:tree-file-content (d:change-after c))))
    (ok (null (d:change-children c)) "a move carries whole nodes")))

(deftest tree-rename-thresholds
  (let ((a `(("src/x.lisp" ,(nl "(a)" "(b)" "(c)" "(d)"))))
        (b `(("lib/x.lisp" ,(nl "(a)" "(X)" "(Y)" "(Z)")))))
    (ok (equal '((:remove ("src" "x.lisp") nil ("tree:/src/x.lisp"))
                 (:add ("lib" "x.lisp") nil ("tree:/lib/x.lisp")))
               (tree-shapes (tdf a b)))
        "similarity 0.25 < 0.5")
    (ok (equal '(:move)
               (kinds (tdf a b :engine (make-instance 'd:tree-diff-engine :rename-threshold 0.2))))))
  (ok (equal '(:remove :add)
             (kinds (tdf (tree-entries-of :nested) (tree-entries-of :renamed)
                         :engine (make-instance 'd:tree-diff-engine :detect-renames nil))))
      "detect-renames nil")
  (ok (equal '(:remove :add) (kinds (tdf '(("e1" "")) '(("e2" "")))))
      "empty files are not rename candidates"))

(deftest tree-rename-pairing
  ;; Exact matches first, removed files in path order taking the first added file
  ;; (path order) with equal content.
  (let ((diff (tdf `(("a" ,(nl "same")) ("b" ,(nl "same")))
                   `(("x" ,(nl "same")) ("y" ,(nl "same"))))))
    (ok (equal '((:move ("x") ("a") ("tree:/a" "tree:/x"))
                 (:move ("y") ("b") ("tree:/b" "tree:/y")))
               (tree-shapes diff)))))

(deftest tree-canonical-order
  (let ((diff (tdf `(("a" ,(nl "x")) ("d" ,(nl "gone")) ("old" ,(nl "1" "2" "3" "4")))
                   `(("a" ,(nl "y")) ("new" ,(nl "1" "2" "3" "4")) ("z" ,(nl "zz"))))))
    (ok (equal '((:remove ("d") nil ("tree:/d"))
                 (:move ("new") ("old") ("tree:/old" "tree:/new"))
                 (:modify ("a" 0) nil ("tree:/a" "text:/a/0-1"))
                 (:add ("z") nil ("tree:/z")))
               (tree-shapes diff))
        "removes, moves, modifies, adds")))

(deftest tree-dir-file-swap
  (let ((diff (tdf (tree-entries-of :nested) (tree-entries-of :dir-to-file))))
    (ok (equal '((:remove ("src" "x.lisp") nil ("tree:/src/x.lisp"))
                 (:remove ("src" "y.lisp") nil ("tree:/src/y.lisp"))
                 (:add ("src") nil ("tree:/src")))
               (tree-shapes diff)))
    (ok (tr= (tr (tree-entries-of :dir-to-file)) (d:apply-diff (tr (tree-entries-of :nested)) diff)))
    (ok (tr= (tr (tree-entries-of :nested))
             (d:apply-diff (tr (tree-entries-of :dir-to-file)) (d:invert-diff diff))))))

(deftest tree-invert-leaf-footprints
  (let* ((diff (tdf `(("f" ,(nl "a" "b" "c"))) `(("f" ,(nl "a" "B" "C" "c")))))
         (inv (d:invert-diff diff)))
    (ok (equal '((:modify ("f" 1) nil ("tree:/f" "text:/f/1-2"))) (tree-shapes diff)))
    (ok (equal '((:modify ("f" 1) nil ("tree:/f" "text:/f/1-3"))) (tree-shapes inv))
        "the text range follows the inverse's before")))

(deftest tree-apply-conflicts
  (flet ((reason (state diff)
           (handler-case (progn (d:apply-diff state diff) nil)
             (d:apply-conflict (e) (d:apply-conflict-reason e)))))
    (let ((add (tdf '() `(("a" ,(nl "x")))))
          (rm (tdf `(("a" ,(nl "x"))) '()))
          (add-deep (tdf '() `(("a/b" ,(nl "x"))))))
      (ok (eq :exists (reason (tr `(("a" ,(nl "other")))) add)))
      (ok (eq :missing (reason (tr '()) rm)))
      (ok (eq :before-mismatch (reason (tr `(("a" ,(nl "other")))) rm)))
      (ok (eq :exists (reason (tr `(("a" ,(nl "file")))) add-deep)) "a file is in the way")
      (ok (tr= (tr `(("a/b" ,(nl "x")) ("c" "k"))) (d:apply-diff (tr '(("c" "k"))) add-deep))
          "missing parents are created")
      (multiple-value-bind (state skipped)
          (handler-bind ((d:apply-conflict #'d:skip-change))
            (d:apply-diff (tr '()) rm))
        (ok (tr= (tr '()) state))
        (ok (= 1 (length skipped)))))))

(deftest tree-sexp-round-trip
  (let* ((a (tr (tree-entries-of :nested)))
         (b (tr (tree-entries-of :renamed-edit)))
         (diff (d:compute-diff *tree* a b :level :content))
         (text (d:render-diff diff :sexp nil))
         (parsed (d:parse-diff :sexp text)))
    (ok (typep (d:diff-engine parsed) 'd:tree-diff-engine))
    (ok (equal text (d:render-diff parsed :sexp nil)))
    (ok (tr= b (d:apply-diff a parsed)))))
