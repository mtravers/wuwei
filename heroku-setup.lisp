(in-package :cl-user)

(asdf:clear-system "wuwei")
(asdf:clear-system "wuwei-examples")

(load (make-pathname :directory *app-dir* :defaults "wuwei.asd"))

(ql:quickload :wuwei-examples)

;;; Redefine / extend heroku-toplevel here if necessary.
