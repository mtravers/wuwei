(in-package :wu)

(defun cl-user::initialize-application ()
  (locate-public-directory "./wupub/")
  (setf *developer-mode* (equal (ccl:getenv "DEVELOPER_MODE") "Y")))

