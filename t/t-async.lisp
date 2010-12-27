(in-package :wu)

;;; An example of the use of ASYNC.

(publish :path "/async-test"
	 :function #'(lambda (req ent)
		       (with-http-response-and-body (req ent)
			 (html
			  (:head
			   (javascript-includes "prototype.js" "effects.js" "dragdrop.js"))
			  (:body
			   (:princ "Async test")
			   (async (:pre-text "Your computation will be here")
			    (let ((i 0))
			      (dotimes (n 100000)
				(incf i (* n n)))
			      (html (:princ "It's ")
				    (:princ i)))))))))			
	 
