(in-package :wu)

;;; Just for testing at the moment -- not impressive enough for an actual demo.
;;; Should exercise some of the extra features we layer on top of the scriptaculous control

(publish :path "/in-place-edit-demo"
	 :content-type "text/html"
	 :function 'in-place-edit-demo)

(publish-code)

(defun in-place-edit-demo (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:head
      (:title "in-place edit demo")
      (javascript-includes "prototype.js" "effects.js" "controls.js" "wuwei.js")
      (css-includes "wuwei.css"))
     (:html 
      ((:body :id "body")
       (example-header #.(this-pathname))
       (:h3 "Autocomplete Demo")
       (:princ "This example shows the use of an in place edit field") :p
       :newline
       (in-place-field)
       :newline
       (tracker)
       )))))
