(defpackage #:puff-cl
  (:use #:cl)
  (:export #:main))

(in-package #:puff-cl)

(defun main ()
  (format t "Hello, Common Lisp!"))

;; read file "examples/addTwo.puff"
