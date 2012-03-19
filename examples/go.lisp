(in-package :wu)

(defun cl-user::initialize-application ()
  (setf *developer-mode* (equal (ccl:getenv "DEVELOPER_MODE") "Y")))

