(defsystem "bnf-tests"
  :author "Jens Thiede"
  :serial t
  :depends-on ("bnf" "xlunit")
  :components ((:file "package")
               (:file "parser-test")))