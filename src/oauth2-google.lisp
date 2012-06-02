(in-package :wb)

;;; For Google, obtain these values at https://code.google.com/apis/console
;;; Gives access to Blogger and Contact services
(defparameter *oauth2-scopes* "http://www.blogger.com/feeds/ https://www.google.com/m8/feeds")
(defparameter *oauth-auth-endpoint* "https://accounts.google.com/o/oauth2/auth")
(defparameter *oauth-token-endpoint* "https://accounts.google.com/o/oauth2/token")

;;; Google specific?

(defun access-protected-resource (url &rest other-args)
  (assert *access-token*)
  (apply #'drakma:http-request url 
	 :additional-headers
	 `(("GData-Version" . "2")	;+++ Google specific, should be controlled by caller
	   ("Authorization" . ,(format nil "Bearer ~A" *access-token*)))
	 other-args))


