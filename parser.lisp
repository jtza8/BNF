(in-package :bnf)

(defun string-set (predicate)
  (lambda (string)
    (loop for i upfrom 0
          for char across string
          while (funcall predicate char)
          finally 
            (return (values (subseq string 0 i)
                            (subseq string i)
                            (not (zerop i)))))))

(defun punctuation-p (char)
  (cl:or (char<= #\! char #\/)
      (char<= #\: char #\@)
      (char<= #\[ char #\`)
      (char<= #\{ char #\~)))

(defun white-space-p (char)
  (characterp (find char '(#\Space #\Tab #\Newline #\Return))))

(defun diget-p (char)
  (char<= #\0 char #\9))

(defun letter-p (char)
  (cl:or (char<= #\A char #\Z)
      (char<= #\a char #\z)))

(defun word (word &optional case-sensitive)
  (lambda (string)
    (loop for i upfrom 0
          for a across word
          for b across string
          while (if (null case-sensitive)
                    (char= (char-upcase a)
                           (char-upcase b))
                    (char= a b))
          finally
            (return (values (subseq string 0 i)
                            (subseq string i)
                            (not (zerop i)))))))

(defun optional (func)
  (lambda (string)
    (multiple-value-bind (word new-string) (funcall func string)
      (values word new-string t))))

(defun sequence (&rest seq)
  (lambda (string)
    (loop with new-string = string
          for func in seq
          for (word tmp-string success) =
              (multiple-value-list 
               (funcall func new-string))
          while success
          do (setf new-string tmp-string)
          collect word into words
          finally 
            (return (if success 
                        (values words new-string t)
                        (values '() string nil))))))

(defun recursion (func)
  (lambda (string)
    (loop with new-string = string
          for (word tmp-string success) =
              (multiple-value-list (funcall func new-string))
          while success
          do (setf new-string tmp-string)
          collect word into words
          finally (return (values words new-string t)))))

(defun or (&rest funcs)
  (lambda (string)
    (loop for func in funcs
          for (word new-string success) =
              (multiple-value-list (funcall func string))
          until success
          finally (return (values word new-string success)))))