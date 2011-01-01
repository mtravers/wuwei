(in-package :wu)

;;; This file contains parameters that are settable for particular applications.

(export '(*system-name* *developer-mode* system-name))

(defparameter *system-name* "WuWei")
(defparameter *developer-mode* nil)
(defparameter *cookie-name* (string+ *system-name* "-session"))
(defparameter *bug-report-url* "https://github.com/mtravers/wuwei/issues")

;;; Timeout for stored Ajax continuations
(defparameter *default-responder-timeout* (* 10 60))

(defun system-name ()
  (if *developer-mode*
      (format nil "~A: ~A" (machine-instance) *system-name*)
      *system-name*))
       
