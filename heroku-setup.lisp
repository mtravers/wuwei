(in-package :cl-user)

(print ">>> Building system....")

;(asdf:clear-system "wuwei")
;(asdf:clear-system "wuwei-examples")

(load (make-pathname :directory *build-dir* :defaults "wuwei.asd"))
;;; This is NOT WORKING, and the build gets Wuwei from Quicklisp.  But that's better than what it was doing before, which was using a completely obsolete version...no fucking idea what is going on here.
(push (make-pathname :directory *build-dir*) asdf:*central-registry*)

(ql:quickload :wuwei-examples)

(in-package :wu)

(defun tracker ()
  "<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-345282-10', 'auto');
  ga('send', 'pageview');

 </script>")

(print ">>> Done building system")
