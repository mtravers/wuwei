(in-package :wu)

;;; Heroku support

(export '(heroku-install-wupub-files wuwei-initialize-application))

;;; Called from an application's heroku-setup.lisp
;;; Directory is a relative path from the app root.
(defun heroku-install-wupub-files (&optional (directory '("wupub")))
  (uiop:run-program
   (format nil "cp -r ~Apublic ~A"
	   (namestring (asdf:component-pathname (asdf:find-system :wuwei)))
	   (namestring (make-pathname :directory (append cl-user::*build-dir* directory))) 
	   )))

;;; Called from cl-user::initialize-application, which is called at startup
(defun wuwei-initialize-application (&key (directory "./wupub/"))
  (locate-public-directory directory)
  (setf *developer-mode* (equal (ccl:getenv "DEVELOPER_MODE") "Y")))  
