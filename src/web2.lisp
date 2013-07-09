(in-package :wu)

;;; Functions that have to be after ajax machinery

;;; Make a <select> element 
(defun select-field (&key id name options url params selected html-options continuations)
  #.(doc "Generate an HTML select field."
	 "If URL is given, trigger off of the mouseup event"
	 "OPTIONS is a list of (value name [title]) tuples")
  (html
    :newline
    ((:select :if* name :name name
              :if* id :id id
;;; Fix for Safari (+++ not really tested)
;;; +++ onchange seems to work better
	      :if* url :onchange (format nil "if (Prototype.Browser.WebKit){~a}" (remote-function url :params (append `(,(keywordize name) (:raw "this.value")) params))) 
	      :if* continuations :onchange (format nil "abortRequests(); new Ajax.Request(this.value, {\"asynchronous\":true,\"parameters\":null});return false;")
	      :do* html-options)
     ;; must be map rather than loop or bindings get recycled...
     (mapc #'(lambda (option)
	       (destructuring-bind (value oname &optional title) option
		 (html
		   ((:option :value (if (functionp value) (ajax-continuation () (funcall value)) value)
			     :if* title :title title
			     :if* (equal value selected) :selected "selected"
			     ;; +++ I don't think this line works
			     :if* url :onmouseup (remote-function url :params (append `(,(keywordize name) ,(format nil "~a" value)) params) :eval-scripts? t)
			     ;;		      :if* (functionp value) :onmouseup
			     )
		    (:princ-safe oname)
		    :newline))))
	   options)
     )))
