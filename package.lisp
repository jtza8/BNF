(defpackage #:bnf
  (:use :cl)
  (:shadow #:or #:sequence)
  (:export #:string-set
           #:punctuation-p
           #:white-space-p
           #:diget-p
           #:letter-p
           #:word
           #:optional
           #:sequence
           #:recursion
           #:or))
