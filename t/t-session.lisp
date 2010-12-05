(in-package :wu)

(defun test-url (s)
  (string+ "http://localhost:8001/tests/" s))

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
