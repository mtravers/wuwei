(in-package :wu)

#|
Illustrates:
- incorporating a 3rd-party Javascript library
- Ajax form handling
|#

(publish :path "/color-demo"
	 :function 'color-demo)

(defun interpolate-colors (c1 c2 p)
  (flet ((interpolate-1 (i)
	   (let* ((cc1 (parse-integer c1 :start i :end (+ i 2) :radix 16))
		  (cc2 (parse-integer c2 :start i :end (+ i 2) :radix 16))
		  (ccn (+ (* cc1 (- 1 p)) (* cc2 p))))
	     (format nil "~2x" (round ccn)))))
    (string+ (interpolate-1 0)
	     (interpolate-1 2)
	     (interpolate-1 4))))
		    
(defun color-demo (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:head 
      (javascript-includes "prototype.js" "effects.js" "jscolor/jscolor.js"))
     (:body
      (:h2 "WuWei Color Demo")
      "Shows off:"
      (:ul
       (:li "Ajax forms and updating")
       (:li "Incorporating a 3rd-party Javascript Library"))
      (let ((continuation 
	     (ajax-continuation (:keep t :args (color1 color2))
	       (render-update
		 (:update "result"
			  (html
			   (:table
			    (:tr
			     (dotimes (i 10)
			       (html 
				(:td
				 ((:div :style (format nil "width:100px;height:100px;background-color:#~A;" (interpolate-colors color1 color2 (/ i 10))))))))))))))))
	(html
	 ((:form :method :post :onsubmit (remote-function continuation :form t))
	  "From this color:"
	  ((:input :name "color1" :class "color" :value "4A6EFF")) :br
	  "To this color:"
	  ((:input :name "color2" :class "color" :value "FFAA54")) :br
	  ((:input :type :submit :value "Interpolate")))
	 ((:div :id "result") "Result goes here")
	 (render-scripts
	   (:js "jscolor.init();"))))))))

	 
