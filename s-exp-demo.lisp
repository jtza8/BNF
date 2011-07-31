; BNF:
; atom := letter [alpha-numeric] | s-exp
; s-exp := "(" [ atom ] { white-space atom } ")"

(defun white-space ()
  (bnf:ignore (bnf:string-set #'bnf:white-space-p)))

(defun an-atom ()
  (bnf:or (bnf:string-set #'bnf:alpha-numeric-p)
          (s-exp)))

(defun s-exp ()
  (bnf:sequence (bnf:ignore (bnf:literal "("))
                (bnf:optional (an-atom))
                (bnf:recursion (bnf:merge (bnf:sequence (white-space)
                                                        (an-atom))))
                (bnf:ignore (bnf:literal ")"))))
  
(defun read-s-exp (string)
  (funcall (s-exp) string))
