(in-package :bnf)

(defclass parser-test (test-case)
  ())

(defparameter *punctuation* (string-set #'punctuation-p))
(defparameter *white-space* (string-set #'white-space-p))
(defparameter *diget* (string-set #'diget-p))
(defparameter *letter* (string-set #'letter-p))
(defparameter *alpha-numeric* (string-set #'alpha-numeric-p))

(def-test-method test-string-sets ((test parser-test))
  (assert-equal ".-@:/\\[]~`{}!" (funcall *punctuation* ".-@:/\\[]~`{}!foo"))
  (assert-equal (format nil "~% 	")
                (funcall *white-space* (format nil "~% 	s")))
  (assert-equal "0123456789" (funcall *diget* "0123456789foo"))
  (assert-equal "fhasdWEkSD" (funcall *letter* "fhasdWEkSD@0123456789foo"))
  (assert-equal "2s3W4t5T6Y" (funcall *alpha-numeric* "2s3W4t5T6Y#f3s")))

(def-test-method test-literal ((test parser-test))
  (assert-equal "FoO" (funcall (literal "foo") "FoOd"))
  (assert-equal "" (funcall (literal "foo" t) "FoOd")))

(def-test-method test-sequence ((test parser-test))
  (assert-equal '(" " "3") (funcall (sequence *white-space* *diget*) " 3"))
  (assert-equal '() (funcall (sequence *white-space* *diget*) "3 "))
  (assert-equal '() (funcall (sequence *white-space* *diget*) " e3")))

(def-test-method test-optional ((test parser-test))
  (let ((func (sequence *diget* (optional *punctuation*) *letter*)))
    (assert-equal '("2" "" "d") (funcall func "2d"))
    (assert-equal '("2" "@" "d") (funcall func "2@d"))))

(def-test-method test-recursion ((test parser-test))
  (let ((func (sequence *letter* (recursion (sequence *white-space*
                                                      *letter*)))))
    (assert-equal '("a" (" " "b") (" " "c")) (funcall func "a b c"))))

(def-test-method test-or ((test parser-test))
  (let ((func (or *white-space* *diget*)))
    (assert-equal " " (funcall func " #"))
    (assert-equal "3" (funcall func "3#"))))

(def-test-method test-merge ((test parser-test))
  (let ((func (merge (sequence *white-space* *diget*))))
    (assert-equal " 3" (funcall func " 3$"))))

(def-test-method test-ignore ((test parser-test))
  (let ((func (merge (sequence (ignore (sequence (literal "#") *white-space*))
                               *alpha-numeric*))))
    (assert-equal "Goo" (funcall func "#  Goo!"))))

(def-test-method test-collapse ((test parser-test))
  (let ((func (collapse (sequence *letter* (recursion (sequence *white-space*
                                                                *letter*))))))
    (assert-equal '("a" " " "b" " " "c") (funcall func "a b c"))))