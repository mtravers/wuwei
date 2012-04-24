(in-package :wu)

;;; An Eval server and page, for development and debugging
;;; Respects *developer-mode*.
;;; On Heroku, turn on off with 
;;; > heroku config:add DEVELOPER_MODE=Y[N]

(publish :path "/eval-server"
	 :function 'eval-server)

(defun eval-server (req ent)
  (if *developer-mode*
      (let* ((form (get-request-body req))
	     (result (multiple-value-list (ignore-errors (eval (read-from-string form))))))
	(if (typep (cadr result) 'error)
	    (with-http-response-and-body (req ent :response *response-bad-request*)
	      (format *html-stream* "Error: ~A" (cadr result)))
	    (with-http-response-and-body (req ent)
	      (write (car result) :stream *html-stream* :readably t))))
      (with-http-response-and-body (req ent :response *response-unauthorized*)
	(write-string "Eval server is not enabled" *html-stream*))))

(publish :path "/eval-page"
	 :function 'eval-page)

(defun eval-page (req ent)
  (with-http-response-and-body (req ent :content-type "text/html")
    (if *developer-mode*
	(html 
	  (:head 
	   (:title "Eval Page")
	   (javascript-includes "prototype.js")
	   (css-include "wuwei.css"))
	  (:body
	   (:h3 "Eval Page")
	   ((:form :method :post 
		   :onsubmit
		   (remote-function 
		    (ajax-continuation (:keep t :args (form))
		      (let ((result (multiple-value-list (ignore-errors (eval (read-from-string form))))))
			(render-update
			  (:update "result"
				   (html
				     (:div
				      (if (typep (cadr result) 'error)
					  (html
					    (:h4 "Error")
					    (:princ-safe (cadr result)))
					  (html 
					    (:h4 "Result")
					    (:princ-safe (prin1-to-string (car result)))))))))
			))
		    :form t))
	    ((:textarea :name "form" :rows 4 :cols 100)) :br
	    ((:input :type :submit :value "Eval"))
	    ((:div :id "result")))))
	(html (:princ "Not enabled.")))))
	   
