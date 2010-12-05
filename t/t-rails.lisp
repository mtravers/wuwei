(in-package :wu)

(defparameter *test-port* 8002)
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

;;; Tests ajax-continuation mechanism via GET-URL
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
			       (ajax-continuation (:no-session? nil) 
						  (setq test t)
						  (render-update (:alert "foo"))))
		      :method :post)))
	(assert-false test)
	;; Should be getting a redirect command back
	(assert-true (search "window.location.href" res)))))

(publish :path "/intro"
	 :function #'(lambda (req ent)
		       (with-http-response-and-body (req ent)
			 (html
			  (:body
			   (:h3 "Welcome to WuWei")
			   ((:img :src (image-url "wuwei.jpg")))
			   (:p
			    ((:a :href "http://en.wikipedia.org/wiki/Wu_wei")
			     (:princ-safe "\"Wu wei wu\""))
			    (:princ-safe " translates to \"effortless doing\" or \"action without action\".  Certainly effortlessness is something for a web toolkit to aspire to!"))
			   (:ul
			    (:li "Continuation-based AJAX user interfaces")
			    (:li "Extensions and fixes to Portable Allegroserve")
			    (:li "Functions for HTML widget generation")
			    (:li "Server-side high-level DOM operations (add/remove elements, visual fades, drag and drop")
			    (:li "Login and session management")

			    ))))))
			   

;;; Generates a page to show off Wuwei render-update tools.  This test requires manual intervention.
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


