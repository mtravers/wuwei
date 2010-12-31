(in-package :wu)

(publish-code)

(publish :path "/render-update-demo" 
	 :function 'render-update-demo)

(defun render-update-demo (req ent)
  (with-http-response-and-body (req ent)
    (html (:head
	   (javascript-includes "prototype.js" "effects.js" "dragdrop.js")
	   (css-includes "wuwei.css"))
	  (:body
	   (example-header #.(this-pathname))
	   (:h1 "WuWei render-update demo")
	   ((:div :id "FOO")
	    (link-to-remote
	     "Click me and I will be replaced"
	     (ajax-continuation () 
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
