(in-package :wu)

;;; Tests of session and login machinery

;;; Test dropping a cookie to establish session
(define-test session-basics
  (publish :path (test-path "session1")
	   :function #'(lambda (req ent)
			 (with-session (req ent)
			   (with-http-response-and-body (req ent) 
			     (html (:html (:body "foo")))))))
  (let ((cookie-jar (make-instance 'net.aserve.client:cookie-jar)))
    (multiple-value-bind (response response-code response-headers)
	(net.aserve.client::do-http-request (test-url "session1")
	  :cookies cookie-jar)
      (declare (ignore response response-headers))
      (assert-equal 200 response-code)
      (assert-true (net.aserve.client::cookie-jar-items cookie-jar)))))

(def-session-variable *test-2* 0)

;;; Test session variable machinery without a web trip
(define-test session-variables
  (let ((*session* (make-new-session nil nil)))
    (with-session-variables
      (assert-equal 0 *test-2*)
      (incf *test-2*))
    (let ((*test-2* nil))
      (with-session-variables
	(assert-equal 1 *test-2*)))))

(def-session-variable *test-session-var* 0)

;;; Test that session state is working (combines the above two)
(define-test session-state
  (publish :path (test-path "session2")
	   :function #'(lambda (req ent)
			 (with-session (req ent)
			   (with-http-response-and-body (req ent) 
			     (html (:html (:body (:princ "v") (:princ *test-session-var*))))
			     (incf *test-session-var*)
			     ))))
  (let ((cookie-jar (make-instance 'net.aserve.client:cookie-jar)))
    (multiple-value-bind (response response-code response-headers)
	(net.aserve.client::do-http-request (test-url "session2")
	  :cookies cookie-jar)
      (declare (ignore response-headers))
      (assert-equal 200 response-code)
      (assert-true (net.aserve.client::cookie-jar-items cookie-jar))
      (assert-true (search "v0" response)))
    (multiple-value-bind (response response-code response-headers)
	(net.aserve.client::do-http-request (test-url "session2")
	  :cookies cookie-jar)
      (declare (ignore response-code response-headers))
      (assert-true (search "v1" response)))
      ))

(defun test-login (req ent)
  (with-http-response-and-body (req ent)
    (render-update
      (:redirect "/login"))))

;;; This is how you do a login.  Note that the make-new-session has to be OUTSIDE the with-http-response-and-body, to allow the cookies to be set early.
(publish :path (test-path "login")
	 :function #'(lambda (req ent)
		       (setq *session* (make-new-session req ent))
		       (with-http-response-and-body (req ent)
			 (html "logged in"))
		       ))
	 

;;; Tests protection of an ajax method against unlogged-in users, and logging in.
(define-test login-required
  (let* ((test nil)
	 (url (format nil "http://localhost:~A~A" 
		      *test-port*
		      (ajax-continuation (:login-handler 'test-login :keep t) 
			(setq test t)
			(render-update (:alert "snockity"))))))
    (let ((res (net.aserve.client:do-http-request url :method :post :query '((:bogus . "value")))))
      (assert-false test)		;should NOT run the continuation
      ;; Should be getting a redirect command back
      (assert-true (search "window.location.href" res)))

    ;; simulate a login and try again
    (let ((cookie-jar (make-instance 'net.aserve.client:cookie-jar)))
      (multiple-value-bind (response response-code response-headers)
	  (net.aserve.client::do-http-request (test-url "login")
	    :cookies cookie-jar)
	(declare (ignore response response-headers))
	(assert-equal 200 response-code)
	(assert-true (net.aserve.client::cookie-jar-items cookie-jar))
	(let ((res (net.aserve.client:do-http-request url :method :post :query '((:bogus . "value"))
						      :cookies cookie-jar)))
	  (assert-true test)
	  (assert-true (search "alert(" res)))
	))))
