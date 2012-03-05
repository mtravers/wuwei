(in-package :cl-user)

(print ">>> Building system.....")

(asdf:clear-system "wuwei")
(asdf:clear-system "wuwei-examples")

(load (make-pathname :directory *app-dir* :defaults "wuwei.asd"))

(ql:quickload :wuwei-examples)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
