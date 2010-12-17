(in-package :wu)

;;; Tests of session and login machinery

(defun test-url (s)
  (string+ "http://localhost:8002/tests/" s))

(defun test-path (s)
  (string+ "/tests/" s))

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
      (assert-equal 200 response-code)
      (assert-true (net.aserve.client::cookie-jar-items cookie-jar)))))

;;; Test session variable machinery without a web trip
(define-test session-variables
  (def-session-variable *test-1* 0)
  (let ((*session* (make-new-session nil nil)))
    (with-session-variables
      (assert-equal 0 *test-1*)
      (incf *test-1*))
    (let ((*test-1* nil))
      (with-session-variables
	(assert-equal 1 *test-1*)))))

;;; Test that session state is working (combines the above two)
(define-test session-state
  (def-session-variable *test-session-var* 0)
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
      (assert-equal 200 response-code)
      (assert-true (net.aserve.client::cookie-jar-items cookie-jar))
      (assert-true (search "v0" response)))
    (multiple-value-bind (response response-code response-headers)
	(net.aserve.client::do-http-request (test-url "session2")
	  :cookies cookie-jar)
      (assert-true (search "v1" response)))
      ))


(defun test-login (req ent)
  (with-http-response-and-body (req ent)
    (render-update
      (:redirect "/login"))))

;;; Tests protection of an ajax method against unlogged-in users
(define-test login-required
  (let* ((test nil)
	 (url (string+ *ajax-test-url* 
		       (ajax-continuation (:session 'test-login :keep t) 
			 (setq test t)
			 (render-update (:alert "snockity"))))))
    (let ((res (net.aserve.client:do-http-request url :method :post)))
      (assert-false test)		;should NOT run the continuation
      ;; Should be getting a redirect command back
      (assert-true (search "window.location.href" res)))

    ;; simulate a login and try again
    (let ((cookie-jar (make-instance 'net.aserve.client:cookie-jar)))
      (multiple-value-bind (response response-code response-headers)
	  (net.aserve.client::do-http-request (test-url "session1")
	    :cookies cookie-jar)
	(assert-equal 200 response-code)
	(assert-true (net.aserve.client::cookie-jar-items cookie-jar))
	(let ((res (net.aserve.client:do-http-request url :method :post
						      :cookies cookie-jar)))
	  (assert-true test)
	  (assert-true (search "alert(" res)))
	))))
