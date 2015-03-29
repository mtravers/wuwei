(in-package :cl-user)

(print ">>> Building system....")

;(asdf:clear-system "wuwei")
;(asdf:clear-system "wuwei-examples")

(load (make-pathname :directory *build-dir* :defaults "wuwei.asd"))
;;; This is NOT WORKING, and the build gets Wuwei from Quicklisp.  But that's better than what it was doing before, which was using a completely obsolete version...no fucking idea what is going on here.
(push (make-pathname :directory *build-dir*) asdf:*central-registry*)

(ql:quickload :wuwei-examples)

(print ">>> Done building system")
