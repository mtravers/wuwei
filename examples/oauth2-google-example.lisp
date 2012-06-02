(in-package :wb)

#|
An example of how to use OAuth.  To run this, you'd have to register your own application with Google, 
and fill in the *oauth2-<foo>* parameters* accordingly. 
|#


(publish :path "/obtain"
	 :content-type "text/html"
	 :function #'(lambda (req ent)
		       (let ((auth-uri (get-auth-code-uri)))
			   (wu:with-http-response-and-body (req ent)
			     (html
			       (render-scripts
				 (:redirect auth-uri)))))))

;;; :::::::::::::::: OAuth callback, gets and displays list of blogs

(publish :path "/oauth2callback"
	 :content-type "text/html"
	 :function 'oauth2callback)

(defun oauth2callback (req ent)
  (let ((error (request-query-value "error" req))
	(code (request-query-value "code" req)))
    (wu:with-session (req ent)
      (with-http-response-and-body (req ent)
	(if error
	    (html 
	     (:h1 "Not authorized")
	     (:princ-safe error))
	    (multiple-value-bind (access-token refresh-token)
		(get-access-token code)
	      ;; we have access token, stash it in session var
	      (setq *access-token* access-token
		    *refresh-token* refresh-token)
	      (let ((result (coerce-to-string (access-protected-resource-with-error "http://www.blogger.com/feeds/default/blogs"))))
		(html
		 (:h2 "Access tokens")
		 "Access: " (:princ-safe *access-token*) :br
		 "Refresh: " (:princ-safe *refresh-token*) :br))
	      ((:h2 "Blogs")
	       (:princ-safe result))))))))
