(in-package :wu)

;;;; Test and example code

;;; Test that te generation machinery is sane
(define-test generation 
    (assert-true 
     (typep
      (html-string
       (link-to-remote "foo" "/foo" :html-options '(:style "font-style:italic") :success "alert('you win');"))
      'string)))

;;; Tests ajax-continuation mechanism
(define-test ajax
    (let ((test nil))
      (get-url (string+ "http://localhost:8002"
			(ajax-continuation (:no-session? t) 
					   (setq test t)
					   (render-update (:alert "foo"))))
	       :method :post)
      (assert-true test)))

;;; same as above, but continuation should not run due to login requirement
(define-test login-required
    (let ((test nil))
      (let ((res
	     (get-url (string+ "http://localhost:8002" 
			       (ajax-continuation () 
						  (setq test t)
						  (render-update (:alert "foo"))))
		      :method :post)))
	(assert-false test)
	;; Should be getting a redirect command back
	(assert-true (search "window.location.href" res)))))

(publish :path "/updated" 
	 :function 'updated-page)

(defun updated-page (req ent)
  (with-http-response-and-body (req ent)
    (html (:head
	   (javascript-includes "prototype.js" "effects.js")
	   )
	  (:body
	   (:h1 "Behold!")
	   ((:div :id "FOO")
	    "I will get replaced")
	   (link-to-remote "Click me" (ajax-continuation () 
							 (render-update
							  (:update "FOO" (html (:i (:princ "I have been replaced")))))))

	   :br
	   ((:a :href "#" :onclick "new Ajax.Request('/fupdation', {contentType: 'text/javascript', asynchronous:true, evalScripts:true});")
	    "Do via server update")
	   :br
	   ((:a :href "#" :onclick "new Ajax.Request('/gupdation', {contentType: 'text/javascript', asynchronous:true, evalScripts:true});")
	    "More tricks")
	   :br
	   "Watch closely"
	   ((:div :id "bar"))
	   (link-to-remote "Even more tricks"
			   (ajax-continuation ()
					      (render-update
					       (:update "FOO" "this is getting cooler")
					       (:update "bar" (html "a " (:b "bold") " advance for computer science"))
					       (:insert :after "bar" (html ((:div :style "background:#FFBDAD; border 1px solid; margin 5px; padding 5x")
									    (:princ (NET.ASERVE::UNIVERSAL-TIME-TO-DATE (get-universal-time))))))
					       )))


	   ((:div :id "dragme" :style "background:#BBFFAD; border: 1px solid; margin: 5px; padding: 5x; width: 50px; height: 50px;"))
	   ((:div :id "notdragme" :style "background:#FFBBAD; border: 1px solid; margin: 5px; padding: 5x; width: 50px; height: 50px;"))
	   (render-scripts
	    (:draggable "dragme"))
	    
	   :br
	   (link-to-remote "Click me" (ajax-continuation ()
							 (render-update 
							  (:delay 2
								  (:insert :after "bar" 
									   (html 
									    ((:div :id "delayed" :style "background:#BDFFAD; border 1px solid; margin 5px; padding 5x")
									     (:princ (NET.ASERVE::UNIVERSAL-TIME-TO-DATE (get-universal-time))))))))))
	   (:princ " to experience some delays") :br
	   (link-to-remote "Click me" (ajax-continuation ()
							 (render-update 
							  (:visual-effect :fade "notdragme"))))
	   (:princ " for some visual effects"
	   )))))

