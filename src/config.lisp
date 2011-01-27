(in-package :wu)

;;; This file contains parameters that are settable for particular applications.

(export '(*system-name* *developer-mode* system-name))

(defparameter *system-name* "WuWei")
(defparameter *developer-mode* nil)
(defparameter *cookie-name* (string+ *system-name* "-session"))		   ;+++ should be done dynamicaly from *system-name*
(defparameter *bug-report-url* "https://github.com/mtravers/wuwei/issues") ;+++ not actually used anywhere.

;;; Timeout for stored Ajax continuations
(defparameter *default-responder-timeout* (* 30 60))

(defun system-name ()
  (if *developer-mode*
      (format nil "~A: ~A" (machine-instance) *system-name*)
      *system-name*))
       
