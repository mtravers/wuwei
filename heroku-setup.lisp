(in-package :cl-user)

(print ">>> Building system....")

(asdf:clear-system "wuwei")
(asdf:clear-system "wuwei-examples")

(load (make-pathname :directory *build-dir* :defaults "wuwei.asd"))

(ql:quickload :wuwei-examples)

(print ">>> Done building system")
