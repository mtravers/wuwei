(in-package :wu)

(publish-code)

;;; An example of the use of ASYNC.

(defun fact (n)
  (if (zerop n) 1
      (* n (fact (- n 1)))))

(publish :path "/async-demo"
	 :content-type "text/html"
	 :function 'async-demo)

(defun async-demo (req ent)
  (with-session (req ent)
    (with-http-response-and-body (req ent)
      (html
	(:head
	 (css-includes "wuwei.css")
	 (javascript-includes "prototype.js" "effects.js" "dragdrop.js"))
	(let ((continuation 
	       (ajax-continuation (:keep t :args (n))
		 (render-update
		   (:update "answer" "")
		   (:hide "error_box"))
		 (let ((n (parse-integer n)))
		   (assert (> n 0) (n) "Must be positive")
		   (render-update 
		     (:update "answer"
			      (async (:pre-text "Wait for it..." :spinner t)
				(html (:princ "It's ")
				      (:princ (log (fact n)))))))))))
	  (html
	    (:body
	     (example-header #.(this-pathname))

	     (:h3 "Async and error handling demo")
	     (:princ "This demo shows off asynchronous results (with the async macro), error handling, and a few assorted other features.  The checkbox selects between two different error handling styles.")
	     :p
	     (checkbox-to-remote "Show errors on page?" 
				 (ajax-continuation (:args (checked) :keep t)
				   (setf *ajax-error-box?* (equal checked "true")))
				 *ajax-error-box?*)
	     :p
	     (:princ (format nil "Compute log(n!):  (try an non-integer argument)"))
	     ((:form :method :post :onsubmit (remote-function continuation :form t))
	      ((:input :name "n" :value "5000"))
	      ((:input :type :submit)))
	     ((:div :id "answer"))
	     ((:div :id "error_box" :style "display:none;"))
	     (tracker)
	     )))))))
