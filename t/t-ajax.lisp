(in-package :wu)

(defparameter *test-port* 8002)
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

(publish :path "/"
	 :function #'(lambda (req ent)
		       (with-http-response-and-body (req ent)
			 (html
			  (:body
			   (:h3 "Welcome to WuWei")
			   ((:img :src (image-url "wuwei.jpg")))
			   (:p
			    ((:a :href "http://en.wikipedia.org/wiki/Wu_wei")
			     (:princ-safe "\"Wu wei wu\""))
			    (:princ-safe " means \"effortless doing\" or \"action without action\".") :br
			    (:princ "Wuwei the software toolkit aims to make making Ajaxified web sites in Lisp as close to effortless as possible."))
			   (:h4 "Features")
			   (:ul
			    (:li "Continuation-based AJAX user interfaces")
			    (:li "Extensions and fixes to Portable Allegroserve")
			    (:li "Functions for HTML widget generation")
			    (:li "Server-side high-level DOM operations (add/remove elements, visual fades, drag and drop")
			    (:li "Login and session management")

			    )
			   (:h4 "Examples")
			   (:ul
			    (:li ((:a :href "/updated") "Demo/test Ajax machinery")))
			   )))))

;;; Tests ajax-continuation mechanism via GET-URL
(define-test ajax
    (let ((test nil))
      (get-url (string+ *ajax-test-url*
			(ajax-continuation () 
					   (setq test t)
					   (render-update (:alert "foo"))))
	       :method :post)
      (assert-true test)))


;;; Generates a page to show off Wuwei render-update tools.  This test requires manual intervention.
(publish :path "/updated" 
	 :function 'updated-page)

(defun updated-page (req ent)
  (with-http-response-and-body (req ent)
    (html (:head
	   (javascript-includes "prototype.js" "effects.js" "dragdrop.js")
	   )
	  (:body
	   (:h1 "WuWei basic tests")
	   ((:div :id "FOO")
	    (link-to-remote "Click me and I will be replaced" (ajax-continuation () 
								(render-update
								  (:update "FOO" (html (:i (:princ "I have been replaced"))))))))

	   :hr
	   ((:div :id "notdragme" :style "background:#FFBBAD; border: 1px solid; margin: 5px; padding: 5x; width: 50px; height: 50px;"))
	   (link-to-remote "Click me" (ajax-continuation ()
					(render-update 
					  (:visual-effect :fade "notdragme"))))
	   " for a disappearing act"

	   :hr

	   (let ((n 1) (acc 1))
	     (link-to-remote "A hand-cranked factorial engine."
			     (ajax-continuation (:keep t)
			     (render-update
			       (:insert :after "bar" (html ((:div :style "background:#FFFFAD; border 1px solid; margin 5px; padding 5x")
							    (setq acc (* acc (incf n)))
							    (:princ (format nil "~A! = ~A" n acc))
							    )))
			       ))))
	   ((:div :id "bar"))
	   :hr
	   "Some drag-n-drop"
	   ((:div :id "dragme" :style "background:#BBFFAD; border: 1px solid; margin: 5px; padding: 5x; width: 100px; height: 50px;")
	    "Drag me")
	   ((:div :id "target" :style "background:#BBFAFD; border: 1px solid; margin: 5px; padding: 5x; width: 100px; height: 50px;")
	    "Drop it here")
	   (render-scripts
	     (:draggable "dragme")
	     (:drop-target "target"
			   :|onDrop| `(:raw "function (elt) {alert('thanks!');}")))
	   
	   

	   ))))


