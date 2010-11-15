(in-package :wu)

(export '(with-session def-session-variable 
	  with-http-response-and-body))

#|
Session management, for now, largely copied from our modified BioBike

Now decoupled from Biobike and user/package convention.

|#

(defvar *sessions* (make-hash-table :test #'eq))

(defparameter *cookie-name* "Wuwei-session") ;feel free to change this for your application

(defun cookie-value (req name)
  (assocdr name (get-cookie-values req) :test #'equal))

;;; Bound by session handler to the session name (a keyword)
(defvar *session* nil)

;;; Note: has to be INSIDE with-http-response-and-body or equiv
;;; PPP um, no, outside, apparently.  Fuck.
(defmacro with-session ((req ent &key login-handler) &body body)
  `(let ((*session* (keywordize (cookie-value ,req *cookie-name*))))
     (unless *session*
       (if ,login-handler
	   (funcall ,login-handler .req ,ent)	;+++ not fleshed out yet PPP
	   (progn
	     (setf *session* (make-new-session req ent)))))
     (with-session-variables 
	 ,@body)))

(defun make-new-session (req ent)
  (declare (ignore ent))
  (let ((*session* (keywordize (gensym "S"))))
    (when req (set-cookie-header req :name *cookie-name* :value (string *session*)))
    (setf (gethash *session* *sessions*) (make-hash-table :test #'eq))
    *session*))
    
;;; PPP removed session and whole-page options; do those through independent mechanisms
(defmacro with-http-response-and-body ((req ent &key  (content-type "text/html")) &body body)
  #.(doc
     "Combines WITH-HTTP-RESPONSE and WITH-HTTP-BODY, which is the"
     "normal way we use those macros.  In doing this we also gain in that"
     "Lispworks will now indent this new macro properly, whereas for some"
     "reason it won't indent WITH-HTTP-RESPONSE or WITH-HTTP-BODY sanely.")
  `(with-http-response (,req ,ent :content-type ,content-type)
     (with-http-body (,req ,ent)
       ,@body)
     ))

;;; Session management

;;; Default value (for new sessions) is simply the symbol's global value, so we don't need to store it anywhere else.

(defvar *session-variables* ())

(defmacro def-session-variable (name &optional initform)
  `(progn
    (defparameter ,name ,initform)
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

(defun session-named (session-key)
  (or (gethash session-key *sessions*)
      (error "Session ~A not found" session-key)))

(defun session-variable-value (session var)
  (gethash var (session-named session) (symbol-value var)))

(defun set-session-variable-value (session var val)
  (setf (gethash var (session-named session)) val))
      
