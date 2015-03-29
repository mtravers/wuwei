(in-package :wu)

(publish-code)

(wu-publish "/arc-challenge"
  (:head 
   (:title "WuWei solution to ARC Challenge")
   (css-include "wuwei.css"))
  (:body
   (example-header #.(this-pathname))
   (:h2 "ARC Challenge Solution")
   "Three solutions to Paul Graham's <a href='http://www.paulgraham.com/arcchallenge.html'>ARC Challenge</a>.  All three are equivalent, but the latter are more compact and/or expressed more straightforwardly."
   (:ul
    (:li ((:a :href "/said") "Basic"))
    (:li ((:a :href "/said2") "Using high-level constructs"))
    (:li ((:a :href "/said3") "With semi-implicit continuations")))))

;;; Before high level macros
(publish :path "/said" 
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response-and-body (req ent)
	       (html
		 ((:form :action (ajax-continuation (:args (foo) :content-type "text/html")
				   (html 
				     ((:a href (wu-continuation ()
						 (html (:princ foo))))
				      "Click me"))))
		  ((:input :name "foo"))
		((:input :type :submit)))))))

;;; With high-level macros
(wu-publish "/said2" 
  ((:form :action (wu-continuation (:args (foo))
		    ((:a href (wu-continuation () (:princ foo)))
		     "Click me")))
   ((:input :name "foo"))
   ((:input :type :submit))))

;;; With partly-implict continuation management
(wu-publish "/said3" 
  (wu-conversation	    
      (() ((:form :action (-next-))
	   ((:input :name "foo"))
	   ((:input :type :submit))))
    ((:args (foo))
     ((:a href (-next-))
      "Click me"))
    (()
     (:princ foo))))






   


