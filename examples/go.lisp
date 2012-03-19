(in-package :wu)

(defun cl-user::initialize-application ()
  (wu:locate-public-directory "./wupub/")
  (setf *developer-mode* (equal (ccl:getenv "DEVELOPER_MODE") "Y")))

