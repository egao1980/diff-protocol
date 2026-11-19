(in-package #:diff-protocol/tests)

;;; R1 :UNIFIED for tree diffs (git style) and PARSE-DIFF :UNIFIED on real `git diff`
;;; output (tests/fixtures/git/*.diff, made by tests/fixtures/make-git-fixtures.sh).

(defun fixture-tree (case side &rest keys)
  (tr (apply #'fixture-entries case side keys)))

(defparameter *renames-after-modes* '(("run.sh" . #o100755)))

;;; Render

(deftest-parametrize git-render-sections
    ((a b expected)
     :ids ("modify" "add" "add-empty" "add-executable" "remove" "pure-rename" "rename-edit"
           "mode" "binary" "no-eol" "two-files-path-order")
     ((tr `(("f" ,(nl "x")))) (tr `(("f" ,(nl "y"))))
      (nl "diff --git a/f b/f" "--- a/f" "+++ b/f" "@@ -1 +1 @@" "-x" "+y"))
     ((tr '()) (tr `(("d/n" ,(nl "x"))))
      (nl "diff --git a/d/n b/d/n" "new file mode 100644" "--- /dev/null" "+++ b/d/n"
          "@@ -0,0 +1 @@" "+x"))
     ((tr '()) (tr '(("e" "")))
      (nl "diff --git a/e b/e" "new file mode 100644"))
     ((tr '()) (tr `(("r" ,(nl "x") :mode #o100755)))
      (nl "diff --git a/r b/r" "new file mode 100755" "--- /dev/null" "+++ b/r" "@@ -0,0 +1 @@" "+x"))
     ((tr `(("f" ,(nl "x" "y")))) (tr '())
      (nl "diff --git a/f b/f" "deleted file mode 100644" "--- a/f" "+++ /dev/null"
          "@@ -1,2 +0,0 @@" "-x" "-y"))
     ((tr `(("a" ,(nl "1" "2")))) (tr `(("b" ,(nl "1" "2"))))
      (nl "diff --git a/a b/b" "similarity index 100%" "rename from a" "rename to b"))
     ((tr `(("a" ,(nl "1" "2" "3" "4")))) (tr `(("b" ,(nl "1" "2" "3" "X"))))
      (nl "diff --git a/a b/b" "similarity index 75%" "rename from a" "rename to b"
          "--- a/a" "+++ b/b" "@@ -4 +4 @@" "-4" "+X"))
     ((tr `(("r" ,(nl "x")))) (tr `(("r" ,(nl "x") :mode #o100755)))
      (nl "diff --git a/r b/r" "old mode 100644" "new mode 100755"))
     ((tr `(("l" ,(octets 1 2)))) (tr `(("l" ,(octets 1 3))))
      (nl "diff --git a/l b/l" "Binary files a/l and b/l differ"))
     ((tr '(("f" "x"))) (tr '(("f" "y")))
      (nl "diff --git a/f b/f" "--- a/f" "+++ b/f" "@@ -1 +1 @@" "-x" *no-eol-marker* "+y" *no-eol-marker*))
     ((tr `(("b" ,(nl "1")) ("a/z" ,(nl "2")))) (tr `(("b" ,(nl "one")) ("a/z" ,(nl "two"))))
      (nl "diff --git a/a/z b/a/z" "--- a/a/z" "+++ b/a/z" "@@ -1 +1 @@" "-2" "+two"
          "diff --git a/b b/b" "--- a/b" "+++ b/b" "@@ -1 +1 @@" "-1" "+one")))
  (dolist (level *levels*)
    (ok (equal expected (d:render-diff (d:compute-diff *tree* a b :level level) :unified nil))
        (format nil "same text at level ~(~A~)" level))))

(deftest git-render-empty
  (ok (equal "" (d:render-diff (d:compute-diff *tree* (tr '()) (tr '())) :unified nil))))

(deftest-parametrize git-render-round-trip
    ((a b level) :ids (tree-pair-ids) :rows (tree-pair-rows))
  ;; A parsed rename has unknown sides, so its "similarity index" line is gone after one
  ;; trip: the round trip is a fixed point from the first re-render on, plus the effect.
  (let* ((diff (d:compute-diff *tree* a b :level level))
         (text (d:render-diff diff :unified nil))
         (parsed (d:parse-diff :unified text))
         (again (d:render-diff parsed :unified nil)))
    (ok (typep (d:diff-engine parsed) 'd:tree-diff-engine) "file headers → a tree diff")
    (ok (equal again (d:render-diff (d:parse-diff :unified again) :unified nil))
        "render ∘ parse is idempotent")
    (unless (search "similarity index" text)
      (ok (equal text again) "without renames: render → parse → render is string="))
    (unless (some (lambda (c) (typep c 'd:opaque-change)) (d:diff-changes parsed))
      (ok (tr= b (d:apply-diff a parsed)) "parsed diff applies"))))

;;; Parse real git output

(deftest git-parse-basic
  (let* ((diff (d:parse-diff :unified (fixture-diff-text "basic")))
         (before (fixture-tree "basic" "before"))
         (after (fixture-tree "basic" "after")))
    (ok (typep (d:diff-engine diff) 'd:tree-diff-engine))
    (ok (eq :changes (d:diff-level diff)))
    (ok (tr= after (d:apply-diff before diff)) "git diff applied to before = after")
    (ok (tr= before (d:apply-diff after (d:invert-diff diff))) "and back")
    (let ((changes (d:diff-changes diff)))
      (flet ((at (path) (remove path changes :key #'d:change-path :test-not #'equal)))
        (ok (equal '(:add) (mapcar #'d:change-kind (at '("docs" "new.md")))) "new file")
        (ok (equal (nl "# New" "" "Doc.")
                   (d:tree-file-content (d:change-after (first (at '("docs" "new.md")))))))
        (ok (equal "" (d:tree-file-content (d:change-after (first (at '("empty.txt"))))))
            "empty new file: header only")
        (ok (equal '(:remove) (mapcar #'d:change-kind (at '("old.txt")))) "deleted file")
        (ok (equal (nl "obsolete") (d:tree-file-content (d:change-before (first (at '("old.txt")))))))
        (ok (equal '(("src" "main.lisp" 7) ("src" "main.lisp" 0))
                   (remove-if-not (lambda (p) (and (equal "main.lisp" (second p)) (cddr p)))
                                  (mapcar #'d:change-path changes)))
            "two @@ hunks, descending, old start - 1")
        (let ((h (find '("win.txt" 0) changes :key #'d:change-path :test #'equal)))
          (ok h "CRLF hunk")
          (ok (equal (crlf "alpha" "beta" "gamma") (apply #'cat (d:change-before h))) "context kept, CR kept")
          (ok (equal '("tree:/win.txt" "text:/win.txt/0-3") (d:change-footprint h))))
        (let ((h (find '("src" "util.lisp" 0) changes :key #'d:change-path :test #'equal)))
          (ok (equal (list (nl "(defun a ())") "(defun b ())") (d:change-before h)) "no newline at end, before")
          (ok (equal (list (nl "(defun a ())") (nl "(defun b ())")) (d:change-after h))))
        (let ((h (find '("notes.txt" 0) changes :key #'d:change-path :test #'equal)))
          (ok (equal (list (nl "one") "two") (d:change-after h)) "no newline at end, after"))))))

(deftest git-parse-renames
  (let* ((diff (d:parse-diff :unified (fixture-diff-text "renames")))
         (before (fixture-tree "renames" "before"))
         (after (fixture-tree "renames" "after" :modes *renames-after-modes*)))
    (ok (tr= after (d:apply-diff before diff)) "renames, edit and mode change apply")
    (let ((changes (d:diff-changes diff)))
      (let ((pure (find '("b.txt") changes :key #'d:change-path :test #'equal)))
        (ok (eq :move (and pure (d:change-kind pure))))
        (ok (equal '("a.txt") (and pure (d:change-from pure))))
        (ok (equal '("tree:/a.txt" "tree:/b.txt") (and pure (d:change-footprint pure))))
        (ok (and pure (not (d:change-before-loaded-p pure))) "content of a parsed rename is unknown"))
      (let ((mv (find '("lib2" "c.lisp") changes :key #'d:change-path :test #'equal)))
        (ok (eq :move (and mv (d:change-kind mv))))
        (ok (equal '("lib" "c.lisp") (and mv (d:change-from mv)))))
      (ok (find '("lib2" "c.lisp" 1) changes :key #'d:change-path :test #'equal)
          "the edit is a hunk at the new path (context from line 2)")
      (let ((m (find (list "run.sh" 'd:tree-mode) changes :key #'d:change-path :test #'equal)))
        (ok (eq :modify (and m (d:change-kind m))) "old mode / new mode")
        (ok (eql #o100644 (and m (d:change-before m))))
        (ok (eql #o100755 (and m (d:change-after m)))))
      (ok (< (position :move changes :key #'d:change-kind)
             (position :modify changes :key #'d:change-kind))
          "canonical order: moves before modifies"))))

(deftest git-parse-binary
  (let* ((diff (d:parse-diff :unified (fixture-diff-text "binary")))
         (bin (find-if (lambda (c) (typep c 'd:opaque-change)) (d:diff-changes diff))))
    (ok bin "Binary files ... differ → an opaque change")
    (ok (eq :modify (and bin (d:change-kind bin))))
    (ok (equal '("tree:/logo.bin") (and bin (d:change-footprint bin))))
    (ok (equal '("logo.bin") (and bin (d:change-path bin))) "a parsed opaque change carries its path")
    (ok (search "Binary files a/logo.bin and b/logo.bin differ" (or (and bin (d:opaque-change-description bin)) "")))
    (ok (find '("t.txt" 0) (d:diff-changes diff) :key #'d:change-path :test #'equal) "the text file next to it")
    (ok (signals (d:apply-diff (fixture-tree "binary" "before" :binary '("logo.bin")) diff)
                 'd:unsupported-content)
        "an opaque change cannot be applied")
    (ok (tr= (fixture-tree "binary" "after" :binary '("logo.bin"))
             (handler-bind ((d:unsupported-content
                              (lambda (c) (declare (ignore c))
                                (use-value (d:make-tree-file (read-file-octets
                                                              (fixture-path "git/binary/after/logo.bin"))
                                                             :type :binary)))))
               (d:apply-diff (fixture-tree "binary" "before" :binary '("logo.bin")) diff)))
        "use-value supplies the node for an opaque change")))

(deftest git-parse-plain-diff-u
  ;; `diff -u before/f.txt after/f.txt`: no "diff --git" line, timestamps after a TAB,
  ;; one leading path component stripped (like patch -p1).
  (let ((diff (d:parse-diff :unified (fixture-diff-text "plain"))))
    (ok (typep (d:diff-engine diff) 'd:tree-diff-engine))
    (ok (equal '(("f.txt" 0)) (mapcar #'d:change-path (d:diff-changes diff))))
    (ok (tr= (fixture-tree "plain" "after") (d:apply-diff (fixture-tree "plain" "before") diff)))))

(deftest git-parse-ignores-noise
  ;; Preamble text, index lines and `diff -r` command lines outside hunks are ignored.
  (let ((diff (d:parse-diff :unified (cat (nl "From: someone" "Subject: patch" "")
                                          (nl "diff --git a/f b/f" "index 1234567..89abcde 100644"
                                              "--- a/f" "+++ b/f" "@@ -1 +1 @@" "-x" "+y")))))
    (ok (tr= (tr `(("f" ,(nl "y")))) (d:apply-diff (tr `(("f" ,(nl "x")))) diff)))))

(deftest git-parse-errors
  (let ((c (handler-case (progn (d:parse-diff :unified (nl "diff --git a/f b/f" "--- a/f" "+++ b/f" "@@ -1,3 +1,3 @@" " x")) nil)
             (d:diff-parse-error (e) e))))
    (ok (typep c 'd:diff-parse-error) "hunk body cut short")
    (ok (eql 6 (and c (d:diff-parse-error-line c))))))
