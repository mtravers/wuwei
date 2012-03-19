(in-package :wu)

(defun cl-user::initialize-application ()
  (locate-public-directory "./public/")
  (setf *developer-mode* (equal (ccl:getenv "DEVELOPER_MODE") "Y")))

