(in-package :wu)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers

;;; Session management

(export '(with-session def-session-variable *default-session* delete-session))

;;; +++ these need to get timed out, otherwise they will accumulate ad infinitum

(defvar *sessions* (make-hash-table :test #'eq))

(defun cookie-value (req name)
  (assocdr name (get-cookie-values req) :test #'equal))

;;; Bound by session handler to the session name (a keyword)
(defvar *session* nil)
(defvar *system-start-time* (princ-to-string (get-universal-time)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-login-handler* nil))

;;; Note: has to be OUTSIDE with-http-response-and-body or equiv
;;; +++ this expands body multiple times, bad.
(defmacro with-session ((req ent &key (login-handler *default-login-handler*)) &body body)
  `(let ((*session* (keywordize (cookie-value ,req *cookie-name*)))
	 (session-time (cookie-value ,req (string+ *cookie-name* "-time"))))
     (cond ((session-named *session* t session-time)
	    (with-session-variables 
	      ,@body))
	   (,login-handler
	    (funcall ,login-handler ,req ,ent)) 
	   (t
	    (setf *session* (make-new-session ,req ,ent))
	    (with-session-variables 
	      ,@body)	    
	    ))))

(defvar *session-counter* 0)

(defun make-new-session (req ent)
  (declare (ignore ent))
  ;; this did use gensym but OpenMCL's implementation is broken.
  (let ((*session* (keywordize (format nil "S~A" (incf *session-counter*)))))
    (when req
      (set-cookie-header req :name *cookie-name* :value (string *session*))
      (set-cookie-header req :name (string+ *cookie-name* "-time") :value (fast-string *system-start-time*)))
    (setf (gethash *session* *sessions*) (make-hash-table :test #'eq))
    *session*))

;;; +++ Should be called from a logout or other state-flushing operation
(defun delete-session (key)
  (remhash key *sessions*))
    
;;; Session management

;;; Default value (for new sessions) is simply the symbol's global value, so we don't need to store it anywhere else.

(defvar *session-variables* ())

(defmacro def-session-variable (name &optional initform)
  `(progn
    (defvar ,name)
    (setf (get ',name :initform) ',initform)
    (pushnew ',name *session-variables*)
    ))

(defmacro with-session-variables (&body body)
  `(progn
     (unless *session* (error "No session"))
     (progv *session-variables*
	 (mapcar #'(lambda (var) (session-variable-value *session* var)) *session-variables*)
       (unwind-protect
	    (progn ,@body)
	 (dolist (v *session-variables*)
	   (set-session-variable-value *session* v (symbol-value v)))))))

(defun session-named (session-key &optional no-error? (time nil time-provided?))
  (cond ((and (or (not time-provided?)
		  (equal time *system-start-time*))
	      (gethash session-key *sessions*)))
	(no-error? nil)
	(t (error "Session ~A not found" session-key))))

(defun session-variable-value (session var)
  (gethash var (session-named session) 
	   (eval (get var :initform))))

(defun set-session-variable-value (session var val)
  (setf (gethash var (session-named session)) val))
      
;;; Developer tools

(publish :path "/session-debug"
	 :function 'session-debug-page)

(defun session-debug-page (req ent)
  (when *developer-mode*
    (with-session (req ent)
      (with-http-response-and-body (req ent)
	(html
	  (:head
	   (css-includes "wuwei.css"))
	  (:h1 "Session State")
	  (:p "Session name: " (:princ *session*))
	  ((:table :border 1)
	   (dolist (v *session-variables*)
	     (html
	       (:tr
		(:td (:princ-safe v))
		(:td (:princ-safe (eval v)))))))
	  (link-to "Reset session" "/session-reset")
	  )))))

(publish :path "/session-reset"
	 :function 
	 #'(lambda (req ent)
	     (when *developer-mode*
	       (make-new-session req ent)
	       (with-http-response-and-body (req ent)
		 (html "session cleared"
		       (render-scripts 
			 (:redirect "/session-debug")))
		 ))))

  
