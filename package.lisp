(defpackage #:bnf
  (:use :cl)
  (:shadow #:or
           #:sequence
           #:merge
           #:ignore)
  (:export #:string-set
           #:punctuation-p
           #:white-space-p
           #:diget-p
           #:letter-p
           #:alpha-numeric-p
           #:word
           #:optional
           #:sequence
           #:recursion
           #:or
           #:merge
           #:ignore))
