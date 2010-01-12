(in-package :wu)

(export '(*system-name* *developer-mode* system-name))

(defvar *system-name* "OncoBike")
(defvar *developer-mode* nil)

(defun system-name ()
  (if *developer-mode*
      (format nil "~A: ~A" (machine-instance) *system-name*)
      *system-name*))
       
