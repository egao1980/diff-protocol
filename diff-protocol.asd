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
               (:file "render"))
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
               (:file "render-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
