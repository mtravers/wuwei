(in-package :wu)

(5am:def-suite :ajax :in :wuwei)
(5am:in-suite :ajax)

(defparameter *test-port* 8003)
;;; aserve started acting hincky in ccl, so this stops working if it is called >1 time...or something like that.
(net.aserve:start :port *test-port*)

(defun test-url (s)
  (format nil "http://localhost:~A/tests/~A" *test-port* s))

(defun test-path (s)
  (string+ "/tests/" s))

;;; Tests for basic Ajax machinery and update generation

;;; Test that the generation machinery is sane
(5am:test generation 
  (5am:is 
     (typep
      (html-string
       (link-to-remote "foo" "/foo" :html-options '(:style "font-style:italic") :success "alert('you win');"))
      'string)))

(defparameter *ajax-test-url* (format nil "http://localhost:~A" *test-port*))

;;; Tests ajax-continuation mechanism via GET-URL
(5am:test ajax
    (let ((test nil))
      (get-url (string+ *ajax-test-url*
			(ajax-continuation () 
					   (setq test t)
					   (render-update (:alert "foo"))))
	       :method :post)
      (5am:is-true test)))





