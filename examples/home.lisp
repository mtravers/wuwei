(in-package :wu)

(defparameter *demo-port* 8001)
(start :port *demo-port*)
(setq *developer-mode* t)		;necessary for the state demo

(defmacro publish-code ()
  (let ((here (this-pathname)))
    (when here
      `(publish-file :file ,here
		     :path (string+ "/code/" ,(pathname-name here) ".lisp")
		     :content-type "text/plain" ))))

(defun example-header (pathname)
  (html
    (:div
     ((:a :href "/") "WuWei Home") (nbsp)
     (when pathname 
       (html ((:a :href (string+ "/code/" (pathname-name pathname) ".lisp") :target "code") "Code"))))))

;;; overwritten on demo site
(defun tracker ()
  )

(publish :path "/"
	 :content-type "text/html"
	 :function
	 #'(lambda (req ent)
	     (with-http-response-and-body (req ent)
	       (html
		 (:head
		  (:title "Welcome to WuWei")
		  (css-includes "wuwei.css"))
		 (:body
		  (:h3 "Welcome to WuWei")
		  (:table
		   (:tr
		    (:td
		     ((:img :src (image-url "wuwei.jpg"))))
		    (:td
		     (:p
		      (:princ "WuWei is a software package for Common Lisp that makes building Ajaxified web sites in Lisp as close to effortless as possible.") :p
		      ((:a :href "http://en.wikipedia.org/wiki/Wu_wei")
		       (:princ-safe "\"Wu wei\""))
		      (:princ-safe " means \"effortless doing\" or \"action without action\".") :p
		      (:princ "The source is hosted at ") ((:a href "https://github.com/mtravers/wuwei/") "GitHub") ".")
		     )))
		  (:h4 "Features")
		  (:ul
		   (:li "Continuation-based AJAX user interfaces")
		   (:li "Server-side high-level DOM operations (add/remove elements, visual fades, drag and drop)")
		   (:li "High-level interfaces to in-place editing and autocomplete widgets")
		   (:li "Login and session management")
		   (:li "Runs in multiple Common Lisp implementations")
		   (:li "Freely available under the MIT Open Source License")
		   )
		  (:h4 "Examples and demos")
		  (:ul
		   (:li ((:a :href "/render-update-demo") "Basic Ajax update machinery"))
		   (:li ((:a :href "/async-demo") "Asynchronus results and error handling"))
		   (:li ((:a :href "/color-demo") "Ajax forms and third party javascript libraries"))
		   (:li ((:a :href "/mql-autocomplete-simple-demo") "Autocomplete field"))
		   (:li ((:a :href "/state-demo") "Session variables and state maintenance"))
		   (:li ((:a :href "/arc-challenge") "Answer to Paul Graham's ARC challenge")))
		  (:h4 "Users")
		  (:ul
		   (:li ((:a :href "http://collabrx.com") "CollabRx"))
		   (:li ((:a :href "http://biocyc.org/") "BioCyc") ))
		  (tracker)
		  )))))



