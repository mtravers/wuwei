(in-package :wu)

(defparameter *test-port* 8003)
;;; aserve started acting hincky in ccl, so this stops working if it is called >1 time...or something like that.
(net.aserve:start :port *test-port*)

;;; Tests for basic Ajax machinery and update generation

;;; Test that the generation machinery is sane
(define-test generation 
  (assert-true 
     (typep
      (html-string
       (link-to-remote "foo" "/foo" :html-options '(:style "font-style:italic") :success "alert('you win');"))
      'string)))

(defparameter *ajax-test-url* (format nil "http://localhost:~A" *test-port*))

;;; Tests ajax-continuation mechanism via GET-URL
(define-test ajax
    (let ((test nil))
      (get-url (string+ *ajax-test-url*
			(ajax-continuation () 
					   (setq test t)
					   (render-update (:alert "foo"))))
	       :method :post)
      (assert-true test)))





