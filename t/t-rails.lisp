(in-package :wu)

(defvar *test-port* 8001)
(net.aserve:start :port *test-port*)

;;; Test and example code

;;; Test that the generation machinery is sane
(define-test generation 
    (assert-true 
     (typep
      (html-string
       (link-to-remote "foo" "/foo" :html-options '(:style "font-style:italic") :success "alert('you win');"))
      'string)))

(defparameter *ajax-test-url* (format nil "http://localhost:~A" *test-port*))

;;; Tests ajax-continuation mechanism
(define-test ajax
    (let ((test nil))
      (get-url (string+ *ajax-test-url*
			(ajax-continuation (:no-session? t) 
					   (setq test t)
					   (render-update (:alert "foo"))))
	       :method :post)
      (assert-true test)))

;;; same as above, but continuation should not run due to login requirement
(define-test login-required
    (let ((test nil))
      (let ((res
	     (get-url (string+ *ajax-test-url* 
			       (ajax-continuation () 
						  (setq test t)
						  (render-update (:alert "foo"))))
		      :method :post)))
	(assert-false test)
	;; Should be getting a redirect command back
	(assert-true (search "window.location.href" res)))))

(publish :path "/updated" 
	 :function 'updated-page)

(setq *default-no-session?* t)

(defun updated-page (req ent)
  (with-http-response-and-body (req ent)
    (html (:head
	   (javascript-includes "prototype.js" "effects.js" "dragdrop.js")
	   )
	  (:body
	   (:h1 "WuWei basic tests")
	   ((:div :id "FOO")
	    "I will get replaced")
	   (link-to-remote "Click me" (ajax-continuation () 
					(render-update
					  (:update "FOO" (html (:i (:princ "I have been replaced")))))))

	   :hr
	   ((:div :id "notdragme" :style "background:#FFBBAD; border: 1px solid; margin: 5px; padding: 5x; width: 50px; height: 50px;"))
	   (link-to-remote "Click me" (ajax-continuation ()
					(render-update 
					  (:visual-effect :fade "notdragme"))))
	   " for a disappearing act"

	   :hr

	   (let ((n 1) (acc 1))
	     (link-to-remote "Factorial the hard way"
			     (ajax-continuation (:keep t)
			     (render-update
			       (:insert :after "bar" (html ((:div :style "background:#FFFFAD; border 1px solid; margin 5px; padding 5x")
							    (setq acc (* acc (incf n)))
							    (:princ (format nil "~A ~A" n acc))
							    )))
			       ))))
	   ((:div :id "bar"))
	   :hr

	   :hr
	   "Some drag-n-drop"
	   ((:div :id "dragme" :style "background:#BBFFAD; border: 1px solid; margin: 5px; padding: 5x; width: 100px; height: 50px;")
	    "Draggable")
	   ((:div :id "target" :style "background:#BBFAFD; border: 1px solid; margin: 5px; padding: 5x; width: 100px; height: 50px;")
	    "Target")
	   (render-scripts
	     (:draggable "dragme")
	     (:drop-target "target"
			   :|onDrop| `(:raw "function (elt) {alert('thanks!');}")))
	   
	   

	   ))))


