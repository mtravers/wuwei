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
	   
;;;; ::::::::::::::::::::::::::::::::  Session debug tool

(publish :path "/session-debug"
	 :function 'session-debug-page)

(defun session-debug-page (req ent)
  (with-session (req ent)
    (with-session-response (req ent)
      (html
       (:head
	(javascript-includes "prototype.js")
	(css-includes "wuwei.css"))
       (:h1 "Session State")
       (if *developer-mode*
	    (html
	     (:h2 "Cookies")
	     ((:table :border 1)
	      (dolist (v (get-cookie-values req))
		(html
		 (:tr
		  (:td (:princ-safe (car v)))
		  (:td (:princ-safe (cdr v)))))))
	     (:h2 "Session state")
	     (:p "Session name: " (:princ *session*))
	     ((:table :border 1)
	      (dolist (v (all-session-variables))
		(html
		 (:tr
		  (:td (:princ-safe (prin1-to-string (session-variable-symbol v))))
		  (:td (:princ-safe (prin1-to-string (mt::return-errors (session-variable-value v)))))))))
	     (flet ((delete-session-cont (type)
		      (ajax-continuation ()
			(with-session (req ent)
			  (delete-session *session* type)
			  (with-session-response (req ent :no-save? t :content-type "text/javascript")
			    (render-update
			      (:redirect "/session-debug")
			      ))))      ))
	       (html
	     (link-to-remote "Delete session" (delete-session-cont nil))
	     :br
	     (link-to-remote "Delete in-memory" (delete-session-cont 'in-memory-session-store))
	     :br
	     (link-to-remote "Delete session cookie" (delete-session-cont 'cookie-session-store)))))
	    (html (:princ "Sorry, not in developer mode")))))))
