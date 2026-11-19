(defsystem "diff-protocol"
  :version "0.1.0"
  :description "CLOS diff protocol for cl-stack: diffs as values (change trees), engines per content type, apply / invert / compose / rebase / three-way"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "sexp-engine")
               (:file "ops")
               (:file "render")
               (:file "text-engine")
               (:file "tree")
               (:file "tree-engine")
               (:file "unified"))
  :in-order-to ((test-op (test-op "diff-protocol/tests"))))

(defsystem "diff-protocol/tests"
  :depends-on ("diff-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "corpus")
               (:file "values-test")
               (:file "sexp-engine-test")
               (:file "laws-test")
               (:file "merge-test")
               (:file "conditions-test")
               (:file "render-test")
               ;; R1
               (:file "r1-corpus")
               (:file "sexp-shift-test")
               (:file "text-engine-test")
               (:file "text-laws-test")
               (:file "text-merge-test")
               (:file "unified-test")
               (:file "tree-test")
               (:file "tree-engine-test")
               (:file "tree-laws-test")
               (:file "git-unified-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
