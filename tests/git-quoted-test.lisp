(in-package #:diff-protocol/tests)

;;; C-quoted paths (diff-protocol#4). Git writes a path in double quotes with C
;;; escapes when it holds a control character, '"' or '\' (always), or a byte
;;; >= #x80 (only with core.quotePath=true, git's default, as \ooo octets of the
;;; UTF-8 name). PARSE-DIFF :UNIFIED unquotes them in `diff --git`, `---` /
;;; `+++`, `rename from` / `rename to` and `copy from` / `copy to` lines;
;;; RENDER-DIFF :UNIFIED quotes them as git does with core.quotePath=off.
;;;
;;; Fixtures: tests/fixtures/git/quoted.diff, quoted-off.diff and copies.diff,
;;; made by tests/fixtures/make-quoted-fixtures.sh with real git. Their trees
;;; cannot be committed (Windows cannot create these names), so they are
;;; spelled out here exactly as the script writes them.

(defun tabbed (&rest parts)
  "PARTS joined with TAB characters."
  (format nil (format nil "~~{~~A~~^~A~~}" #\Tab) parts))

(defparameter *cafe* (coerce (list #\c #\a #\f (code-char #xE9) #\. #\t #\x #\t) 'string)
  "\"café.txt\" (U+00E9).")

(defparameter *nl-name* (format nil "nl~%name.txt"))

(defparameter *quoted-bin* (octets 0 1 2))

(defun quoted-before ()
  (tr `((,(tabbed "tab" "here.txt") ,(nl "one" "two"))
        ("quote\"d.txt" ,(nl "1" "2" "3" "4" "5"))
        ("plain.txt" ,(nl "a" "b" "c" "d" "e"))
        ("back\\slash.txt" ,(nl "gone"))
        (,*cafe* ,(nl "x"))
        (,(tabbed "mode" "x.sh") ,(nl "echo")))))

(defun quoted-after ()
  (tr `((,(tabbed "tab" "here.txt") ,(nl "one" "TWO"))
        (,(tabbed "renamed" "too.txt") ,(nl "1" "2" "3" "4" "FIVE"))
        ("new\"name.txt" ,(nl "a" "b" "c" "d" "e"))
        (,*cafe* ,(nl "y"))
        (,*nl-name* ,(nl "new"))
        (,(tabbed "bin" "file.bin") ,*quoted-bin* :type :binary)
        (,(tabbed "mode" "x.sh") ,(nl "echo") :mode #o100755))))

(defun read-file-utf-8 (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (with-output-to-string (out)
      (loop for c = (read-char in nil nil) while c do (write-char c out)))))

(defun quoted-fixture (name)
  ;; quoted.diff is ASCII; quoted-off.diff holds the raw UTF-8 of "café".
  (read-file-utf-8 (fixture-path (format nil "git/~A.diff" name))))

(defun apply-supplying-binary (tree diff)
  "APPLY-DIFF, answering the opaque add of the binary file with its node."
  (handler-bind ((d:unsupported-content
                   (lambda (c) (declare (ignore c))
                     (use-value (d:make-tree-file *quoted-bin* :type :binary)))))
    (d:apply-diff tree diff)))

(defun change-at (path diff)
  (find path (d:diff-changes diff) :key #'d:change-path :test #'equal))

;;; Parse real git output

(deftest-parametrize git-parse-quoted-paths
    ((fixture) :ids ("quotepath-true" "quotepath-off")
     ("quoted") ("quoted-off"))
  (let ((diff (d:parse-diff :unified (quoted-fixture fixture))))
    (ok (tr= (quoted-after) (apply-supplying-binary (quoted-before) diff))
        "git diff applied to before = after")
    (testing "unquoted paths"
      (ok (change-at (list (tabbed "tab" "here.txt") 0) diff) "\\t in diff --git / --- / +++")
      (let ((rm (change-at '("back\\slash.txt") diff)))
        (ok (eq :remove (and rm (d:change-kind rm))) "\\\\ in a deleted file"))
      (let ((add (change-at (list *nl-name*) diff)))
        (ok (eq :add (and add (d:change-kind add))) "\\n in an added file")
        (ok (equal (nl "new") (and add (d:tree-file-content (d:change-after add))))))
      (ok (change-at (list *cafe* 0) diff) "non-ASCII: \\303\\251 octets or raw UTF-8")
      (let ((pure (change-at '("new\"name.txt") diff)))
        (ok (eq :move (and pure (d:change-kind pure))) "unquoted old, quoted new")
        (ok (equal '("plain.txt") (and pure (d:change-from pure)))))
      (let ((mv (change-at (list (tabbed "renamed" "too.txt")) diff)))
        (ok (eq :move (and mv (d:change-kind mv))) "rename from / to, both quoted")
        (ok (equal '("quote\"d.txt") (and mv (d:change-from mv))))
        (ok (equal (list "tree:/quote\"d.txt" (cat "tree:/" (tabbed "renamed" "too.txt")))
                   (and mv (d:change-footprint mv)))))
      (ok (change-at (list (tabbed "renamed" "too.txt") 1) diff) "the rename's hunk at the new path")
      (let ((m (change-at (list (tabbed "mode" "x.sh") 'd:tree-mode) diff)))
        (ok (eql #o100755 (and m (d:change-after m))) "mode change of a quoted path"))
      (let ((bin (find-if (lambda (c) (typep c 'd:opaque-change)) (d:diff-changes diff))))
        (ok (equal (list (tabbed "bin" "file.bin")) (and bin (d:change-path bin)))
            "Binary files … differ under a quoted header")))))

(deftest git-parse-quoted-fixtures-agree
  ;; Only the quoting of non-ASCII differs between the two fixtures.
  (flet ((paths (name)
           (mapcar (lambda (c) (list (d:change-kind c) (d:change-path c) (d:change-from c)))
                   (d:diff-changes (d:parse-diff :unified (quoted-fixture name))))))
    (ok (equal (paths "quoted") (paths "quoted-off")))))

(deftest git-parse-copies
  ;; `copy from` / `copy to`: the copy's content is the source's, which a patch does
  ;; not carry. It parses as an opaque :ADD of the destination (footprint: source,
  ;; destination) that APPLY-DIFF answers through USE-VALUE, like a binary file.
  (let* ((diff (d:parse-diff :unified (quoted-fixture "copies")))
         (changes (d:diff-changes diff))
         (cp (first changes)))
    (ok (= 1 (length changes)))
    (ok (typep cp 'd:opaque-change))
    (ok (eq :add (d:change-kind cp)))
    (ok (equal (list (tabbed "copy" "file.txt")) (d:change-path cp)))
    (ok (equal (list (tabbed "src" "file.txt")) (d:change-from cp)))
    (ok (equal (list (cat "tree:/" (tabbed "src" "file.txt")) (cat "tree:/" (tabbed "copy" "file.txt")))
               (d:change-footprint cp)))
    (let ((before (tr `((,(tabbed "src" "file.txt") ,(nl "same" "content"))))))
      (ok (signals (d:apply-diff before diff) 'd:unsupported-content))
      (ok (tr= (tr `((,(tabbed "src" "file.txt") ,(nl "same" "content"))
                     (,(tabbed "copy" "file.txt") ,(nl "same" "content"))))
               (handler-bind ((d:unsupported-content
                                (lambda (c) (declare (ignore c))
                                  (use-value (d:make-tree-file (nl "same" "content") :type :text)))))
                 (d:apply-diff before diff)))))
    (ok (equal (remove-if (lambda (l) (search "similarity index" l))
                          (lines-of (quoted-fixture "copies")))
               (lines-of (d:render-diff diff :unified nil)))
        "a parsed copy renders back as git wrote it, less the similarity index")))

;;; Escapes, one at a time

(deftest-parametrize git-parse-c-escapes
    ((quoted expected) :ids ("tab" "newline" "quote" "backslash" "bell-to-cr" "octal-utf-8"
                             "octal-control" "non-utf-8-byte" "space-kept")
     ("a\\tb" (tabbed "a" "b"))
     ("a\\nb" (format nil "a~%b"))
     ("a\\\"b" "a\"b")
     ("a\\\\b" "a\\b")
     ("\\a\\b\\f\\r\\v" (coerce (mapcar #'code-char '(7 8 12 13 11)) 'string))
     ("caf\\303\\251.txt" *cafe*)
     ("x\\001y" (coerce (list #\x (code-char 1) #\y) 'string))
     ("\\377" (string (code-char 255)))
     ("a b" "a b"))
  (let* ((text (nl (format nil "diff --git \"a/~A\" \"b/~A\"" quoted quoted)
                   (format nil "--- \"a/~A\"" quoted)
                   (format nil "+++ \"b/~A\"" quoted)
                   "@@ -1 +1 @@" "-x" "+y"))
         (expected (if (symbolp expected) (symbol-value expected) expected))
         (diff (d:parse-diff :unified text)))
    (ok (equal (list (list expected 0)) (mapcar #'d:change-path (d:diff-changes diff))))))

(deftest-parametrize git-parse-bad-quoting
    ((line) :ids ("unterminated-header" "unknown-escape" "octal-out-of-range" "unterminated-minus"
                  "unterminated-rename")
     ("diff --git \"a/x b/x")
     ("diff --git \"a/\\q\" \"b/\\q\"")
     ("diff --git \"a/\\400\" \"b/\\400\"")
     ("--- \"a/x")
     ("rename from \"x"))
  (let* ((text (if (search "rename" line)
                   (nl "diff --git a/x b/y" line "rename to y")
                   (if (search "---" line)
                       (nl "diff --git a/x b/x" line "+++ b/x" "@@ -1 +1 @@" "-x" "+y")
                       (nl line "--- a/x" "+++ b/x" "@@ -1 +1 @@" "-x" "+y"))))
         (c (handler-case (progn (d:parse-diff :unified text) nil)
              (d:diff-parse-error (e) e))))
    (ok (typep c 'd:diff-parse-error) "a malformed quoted path is a parse error")
    (ok (equal line (and c (d::%diff-parse-error-text c))) "naming the line")))

;;; Render quotes as git does with core.quotePath=off

(deftest git-render-quoted-paths
  (let* ((diff (d:compute-diff *tree* (quoted-before) (quoted-after)))
         (text (d:render-diff diff :unified nil))
         (git (quoted-fixture "quoted-off")))
    (dolist (line (list "diff --git \"a/tab\\there.txt\" \"b/tab\\there.txt\""
                        "--- \"a/tab\\there.txt\""
                        "+++ \"b/tab\\there.txt\""
                        "diff --git \"a/back\\\\slash.txt\" \"b/back\\\\slash.txt\""
                        "--- \"a/back\\\\slash.txt\""
                        "+++ \"b/nl\\nname.txt\""
                        "diff --git a/plain.txt \"b/new\\\"name.txt\""
                        "rename to \"new\\\"name.txt\""
                        "diff --git \"a/quote\\\"d.txt\" \"b/renamed\\ttoo.txt\""
                        "rename from \"quote\\\"d.txt\""
                        "rename to \"renamed\\ttoo.txt\""
                        "diff --git \"a/mode\\tx.sh\" \"b/mode\\tx.sh\""
                        "Binary files /dev/null and \"b/bin\\tfile.bin\" differ"
                        (format nil "diff --git a/~A b/~A" *cafe* *cafe*)))
      (ok (search (nl line) git) (format nil "git wrote ~S" line))
      (ok (search (nl line) text) (format nil "render writes ~S" line)))
    (let ((parsed (d:parse-diff :unified text)))
      (ok (tr= (quoted-after) (apply-supplying-binary (quoted-before) parsed))
          "render → parse → apply"))))
