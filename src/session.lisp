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

(export '(cookie-value
	  with-session def-session-variable delete-session new-session-hook
	  *aserve-request*))

;;; +++ these need to get timed out, otherwise they will accumulate ad infinitum

(defvar *sessions* (make-hash-table :test #'eq))

(defun cookie-value (req name)
  (assocdr name (get-cookie-values req) :test #'equal))

;;; Bound by session handler to the session name (a keyword)
(defvar *session* nil)
(defvar *system-start-time* (get-universal-time))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-login-handler* nil))

(defparameter *cookie-name* (string+ *system-name* "-session"))

;;; Dynamic bound to current request, makes life much easier
;;; Bound in with-session, but should be universal
;;; There is an aserve variable that is probably better to use, net.aserve::*worker-request* +++
(defvar *aserve-request* nil)

;;; Note: has to be OUTSIDE with-http-response-and-body or equiv
;;; +++ this expands body multiple times, bad.
(defmacro with-session ((req ent &key (login-handler *default-login-handler*)) &body body)
  `(let ((*aserve-request* ,req)
	 (*session* (parse-and-validate-cookie (cookie-value ,req *cookie-name*))))
     (cond ((session-named *session* t)
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
    
;;; Session management

;;; Default value (for new sessions) is simply the symbol's global value, so we don't need to store it anywhere else.

(defvar *session-variables* ())

(defmacro def-session-variable (name &optional initform)
  `(progn
    (defvar ,name ,initform)
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

(defun session-named (session-key &optional no-error?)
  (cond ((gethash session-key *sessions*))
	(no-error? nil)
	(t (error "Session ~A not found" session-key))))

(defun session-variable-value (session var)
  (gethash var (session-named session) 
	   (eval (get var :initform))))

(defun set-session-variable-value (session var val)
  (setf (gethash var (session-named session)) val))
      
(defun make-new-session (req ent)
  ;; this did use gensym but OpenMCL's implementation is broken.
  (let ((*session* (keywordize (format nil "S~A" (incf *session-counter*)))))
    (when req
      (set-cookie-header req :name *cookie-name* :value (generate-session-cookie) :expires :never))
    (setf (gethash *session* *sessions*) (make-hash-table :test #'eq))
    (with-session-variables
      (new-session-hook req ent))
    *session*))

;;; New secure cookies. 
#|
Theory: there's one cookie that points to the session state on the server.  The cookie is of the form:
    <session-key>|<system-start-time>|<hash>
The hash prevents forgery; the system-start-time ensures that if the server is restarted all saved session cookies will
be invalidated.

This may be wrong. Maybe credentials should also be stored; so if the server is rebooted the user isn't forced to log
in again.

|#

(defun string-md5 (string)
  #+ALLEGRO (let ((*print-base* 16)) (princ-to-string (excl:md5-string string)))
  #-ALLEGRO (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :md5 string)))

(defun generate-session-cookie ()
  (let* ((part1 (format nil "~A|~X" *session* *system-start-time*))
	 (hash (string-md5 (string+ part1 *session-secret*))))
    (format nil "~A|~A" part1 hash)))

;;; Input: cookie value, output: session keyword
(defun parse-and-validate-cookie (value)
  (when value
    (report-and-ignore-errors 
      (let* ((parts (string-split value #\|)))
	(unless (and (= 3 (length parts))
		     (equal (third parts)
			    (string-md5 (string+ (first parts) "|" (second parts) *session-secret*))))
	  (error "Invalid session cookie ~A" value))
	(unless (= (parse-integer (second parts) :radix 16)
		   *system-start-time*)
	  (error "Expired session cookie ~A" value))
	(keywordize (first parts))))))

;;; applications can redefine this to do special actions to initialize a session
(defun new-session-hook (req ent)
  (declare (ignore req ent))
  )

;;; +++ Should be called from a logout or other state-flushing operation
(defun delete-session (key)
  (remhash key *sessions*))

;;; Developer tools

(publish :path "/session-debug"
	 :function 'session-debug-page)

(defun session-debug-page (req ent)
  (with-session (req ent)
    (with-http-response-and-body (req ent)
      (html
       (:head
	(css-includes "wuwei.css"))
       (:h1 "Session State")
       (if *developer-mode*
	    (html
	     (:h2 "Cookies")
	     ((:table :border 1)
	      (dolist (v (get-cookie-values req))
		(html
		 (:tr
		  (:td (:princ-safe (car v)))
		  (:td (:princ-safe (cdr v)))))))
	     (:h2 "Session state")
	     (:p "Session name: " (:princ *session*))
	     ((:table :border 1)
	      (dolist (v *session-variables*)
		(html
		 (:tr
		  (:td (:princ-safe (prin1-to-string v)))
		  (:td (:princ-safe (prin1-to-string (eval v))))))))
	     (link-to "Reset session" "/session-reset")
	     )
	    (html (:princ "Sorry, not in developer mode")))))))


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

  
