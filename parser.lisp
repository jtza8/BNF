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

(defun alpha-numeric-p (char)
  (cl:or (diget-p char)
         (letter-p char)))

(defun literal (word &optional case-sensitive)
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
              (multiple-value-list (funcall func new-string))
          while success
          do (setf new-string tmp-string)
          if (and (listp word) (null (car word)))
            append (cdr word) into words
          else
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
          finally (return (values (cons nil words) new-string t)))))

(defmacro or (&rest funcs)
  (let ((word (gensym "WORD-"))
        (new-string (gensym "NEW-STRING-"))
        (success (gensym "SUCCESS-")))
    `(lambda (string)
       (let (,word ,new-string ,success)
         (tagbody
            ,@(loop for func in funcs
                    collect `(setf (values ,word ,new-string ,success)
                                   (funcall ,func string))
                    collect `(when ,success (go end)))
          end)
         (values ,word ,new-string ,success)))))

(defun merge (func)
  (lambda (string)
    (multiple-value-bind (word string success) (funcall func string)
      (if (and success (every #'stringp word))
          (values (apply #'concatenate 'string word) string t)
          (values word string nil)))))

(defun ignore (func)
  (lambda (string)
    (values-list (cons nil (cdr (multiple-value-list (funcall func string)))))))

(defun collapse (func)
  (lambda (string)
    (multiple-value-bind (word string success) (funcall func string)
      (values
       (loop for item in word
             when (listp item)
               append item
             else
               collect item)
          string
          success))))