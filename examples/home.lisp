(in-package :wu)

(defparameter *demo-port* 8004)
(start :port *demo-port*)

(publish :path "/"
	 :function
	 #'(lambda (req ent)
	     (with-http-response-and-body (req ent)
	       (html
		(:head
		 (:title "Welcome to WWuWei")
		 (css-includes "wuwei.css"))
		(:body
		 (:h3 "Welcome to WuWei")
		 (:table
		  (:tr
		   (:td
		    ((:img :src (image-url "wuwei.jpg"))))
		   (:td
		    (:p
		     ((:a :href "http://en.wikipedia.org/wiki/Wu_wei")
		      (:princ-safe "\"Wu wei\""))
		     (:princ-safe " means \"effortless doing\" or \"action without action\".") :p
		     (:princ "Wuwei is a software package for Common Lisp that makes building Ajaxified web sites in Lisp as close to effortless as possible.") :p
		     (:princ "The source is hosted at ") ((:a href "https://github.com/mtravers/wuwei/") "GitHub") ".")
		    )))
		 (:h4 "Features")
		 (:ul
		  (:li "Continuation-based AJAX user interfaces")
		  (:li "Server-side high-level DOM operations (add/remove elements, visual fades, drag and drop")
		  (:li "Extensions and fixes to Portable Allegroserve")
		  (:li "High-level interfaces to in-place-editing and autocomplete widgets")
		  (:li "Login and session management")
		  )
		 (:h4 "Examples and demos")
		 (:ul
		  (:li ((:a :href "/render-update-demo") "Basic Ajax update machinery"))
		  (:li ((:a :href "/color-demo") "Ajax forms and third party javascript libraries"))
		  (:li ((:a :href "/mql-autocomplete-simple-demo") "Autocomplete field (and Freebase API)")))
		 )))))



