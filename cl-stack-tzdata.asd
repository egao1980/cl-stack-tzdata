(defsystem "cl-stack-tzdata"
  :version "2026.3.0"
  :description "IANA tzdb as a Lisp-loadable package (TZif + aliases + abbreviation index)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "tzif")
               (:file "posix-tz")
               (:file "repository"))
  :in-order-to ((test-op (test-op "cl-stack-tzdata/tests"))))

(defsystem "cl-stack-tzdata/tests"
  :depends-on ("cl-stack-tzdata" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "tzif-test")
               (:file "repository-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
