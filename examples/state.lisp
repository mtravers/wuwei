(in-package :wu)

(publish-code)

(publish :path "/state-demo" 
	 :content-type "text/html"
	 :function 'state-demo)

(def-session-variable *demo-counter* 0)

(defun state-demo (req ent)
  (with-session (req ent)
    (with-http-response-and-body (req ent)
      (html (:head
	     (javascript-includes "prototype.js" "effects.js" "dragdrop.js")
	     (css-includes "wuwei.css"))
	    (:body
	     (example-header #.(this-pathname))
	     (:h1 "Two kinds of state demo")
	     "This counter is maintained in a continuation (so will get reset on a page refresh)"
	     :br
	     (let ((i 0))
	       (html
		 ((:span :id "i_c") (:princ i))
		 (nbsp)
		 (link-to-remote
		  "Increment"
		  (ajax-continuation (:keep t) 
		    (render-update
		      (:update "i_c" (html (:princ (incf i))))))))

	       (html
		 :p
		 "This counter is maintained in a session variable, so will persist across page refreshes and browser restarts.  "
		 (when *developer-mode*
		   (html ((:a :href "/session-debug") "See or reset the session state")))
		 :br
		 ((:span :id "s_c") (:princ *demo-counter*))
		 (nbsp)
		 (link-to-remote
		  "Increment"
		  (ajax-continuation (:keep t) 
		    (render-update
		      (:update "s_c" (html (:princ (incf *demo-counter*)))))))))
	     (tracker)
	     )))))

	   

