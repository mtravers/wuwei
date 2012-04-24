(in-package :wu)

;;; This file contains parameters that are settable for particular applications.

(export '(*system-name* *developer-mode* system-name))

(defparameter *system-name* "WuWei")
(defparameter *developer-mode* nil)	;Warning: setting this true turns on an eval server that is a security risk
(defparameter *bug-report-url* "https://github.com/mtravers/wuwei/issues") 
(defparameter *session-secret* "barbie says: security is hard")		   ;For hashing session cookies. Should be set to something unique for each server.

;;; Timeout for stored Ajax continuations
(defparameter *default-responder-timeout* (* 30 60))

(defun system-name ()
  (if *developer-mode*
      (format nil "~A: ~A" (machine-instance) *system-name*)
      *system-name*))

