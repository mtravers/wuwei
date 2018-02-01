(in-package :wu)

;;; Heroku support

(export '(heroku-install-wupub-files wuwei-initialize-application))

;;; Called from an application's heroku-setup.lisp
;;; Directory is a relative path from the app root.
(defun heroku-install-wupub-files (&optional (directory '("wupub")))
  (uiop:run-program
   `("cp" "-r" ,(uiop:native-namestring (asdf:system-relative-pathname "wuwei" "public"))
          ,(uiop:native-namestring (uiop:subpathname cl-user::*build-dir* directory)))))

;;; Called from cl-user::initialize-application, which is called at startup
(defun wuwei-initialize-application (&key (directory "./wupub/"))
  (locate-public-directory directory)
  (setf *developer-mode* (equal (uiop:getenv "DEVELOPER_MODE") "Y")))
