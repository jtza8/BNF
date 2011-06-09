; BNF:
; atom := letter [alpha-numeric] | s-exp
; s-exp := "(" atom { white-space atom } ")"

(defun white-space ()
  (bnf:ignore (bnf:string-set #'bnf:white-space-p)))

(defun my-atom ()
  (bnf:string-set #'bnf:alpha-numeric-p))

(defun s-exp ()
  (bnf:sequence (bnf:word "(") (my-atom)
                (bnf:recursion 
                 (bnf:merge
                  (bnf:sequence (white-space)
                                (my-atom))))
                (bnf:word ")")))

(defun read-s-exp (string)
  (funcall (my-atom) string))
