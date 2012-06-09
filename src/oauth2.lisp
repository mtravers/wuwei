(in-package :wu)

;;; These values need to be defined for the particular service you want to access
;;; TODO: encapsulate in an object to make it possible to deal with multiple services.
(defparameter *oauth2-client-id* nil)
(defparameter *oauth2-client-secret* nil)
(defparameter *oauth2-callback* "")
(defparameter *oauth2-scopes* nil)
(defparameter *oauth-auth-endpoint* nil)
(defparameter *oauth-token-endpoint* nil)

;;; The obtained tokens are maintained in WuWei session variables
(def-session-variable *access-token* nil)
(def-session-variable *refresh-token* nil) 

;;; This URI gets passed to client through a redirect
(defun get-auth-code-uri ()
  (let ((endpoint (puri:uri *oauth-auth-endpoint*))
	(parameters 
	 `(("response_type" . "code")
	   ("client_id" . ,*oauth2-client-id*)
	   ("redirect_uri" . ,*oauth2-callback* )
	   ("scope" .  ,*oauth2-scopes*) 
	   ("access_type" . "offline"))))
    (setf (puri:uri-query endpoint)
	  (drakma::alist-to-url-encoded-string parameters :latin1))
    (puri:render-uri endpoint nil)))

(defun coerce-drakma-to-string (ss)
  (if (stringp ss)
      ss
      ;; alternatively 
      ;; (babel:octets-to-string ss :encoding :iso-8859-1)))
      (flexi-streams:octets-to-string ss)))

(defun get-access-token (code)
  (let ((parameters `(("code" . ,code)
		      ("client_id" . ,*oauth2-client-id*)
		      ("client_secret" . ,*oauth2-client-secret*)
		      ("redirect_uri" . ,*oauth2-callback*) 
		      ("grant_type" . "authorization_code"))))
    (multiple-value-bind (body status) ;...
	(drakma:http-request *oauth-token-endpoint*
			     :method :post
			     :parameters parameters)
      (let ((json-response
	     (json::decode-json-from-string 
	       (coerce-drakma-to-string body))))
      (if (= status 200)
	  (let ((access-token (cdr (assoc :ACCESS--TOKEN json-response)))
		(refresh-token (cdr (assoc :REFRESH--TOKEN json-response))))
	    (values access-token refresh-token))
	  (error "Failed to get access token ~A ~A" status json-response))))))

;;; access-protected-resource needs to be defined on a server-specific basis, AFAIKT
;;; This gets redefined.
(defun access-protected-resource (url &rest ignore)
  (declare (ignore url ignore))
  (error "access-protected-resource needs to be defined"))

(defun access-protected-resource-with-error (url &rest other-args)
  (multiple-value-bind  (result status prob-hint prob-advice)
      (apply #'access-protected-resource url other-args)
    (case status
      (200 result)
      (t (error "Failed to get protected resource ~A: ~A ~A ~A ~A" url status result prob-hint prob-advice)))))
