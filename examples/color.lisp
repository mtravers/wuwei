(in-package :wu)

#|
Illustrates:
- incorporating a 3rd-party Javascript library
- Ajax form handling
|#

(publish :path "/color-demo"
	 :content-type "text/html"
	 :function 'color-demo)

(publish-code)

(defun interpolate-colors (c1 c2 p)
  (flet ((interpolate-1 (i)
	   (let* ((cc1 (parse-integer c1 :start i :end (+ i 2) :radix 16))
		  (cc2 (parse-integer c2 :start i :end (+ i 2) :radix 16))
		  (ccn (+ (* cc1 (- 1 p)) (* cc2 p))))
	     (format nil "~2,'0x" (round ccn)))))
    (string+ (interpolate-1 0)
	     (interpolate-1 2)
	     (interpolate-1 4))))
		    
(defun color-demo (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:head 
      (:title "WuWei Color Demo")
      (javascript-includes "prototype.js" "effects.js" "jscolor/jscolor.js")
      (css-include "wuwei.css"))
     (:body
      (example-header #.(this-pathname))
      (:h2 "WuWei Color Demo")
      "Shows off:"
      (:ul
       (:li "Ajax forms and updating")
       (:li "Incorporating a 3rd-party Javascript Library"))
      (let ((continuation 
	     (ajax-continuation (:keep t :args (color1 color2 n))
	       (let ((n (or (ignore-errors (parse-integer n)) 24)))
		 (render-update
		   (:update "result"
			    (html
			     (:table
			      (dotimes (row (ceiling n 8))
				(html
				 (:tr
				  (dotimes (col 8)
				    (let ((i (+ col (* row 8) )))
				      (when (< i n)
					(html 
					 (:td
					  ((:div :style (format nil "width:50px;height:50px;background-color:#~A;" (interpolate-colors color1 color2 (/ i (- n 1))))))))))))))))))))))
	(html
	 ((:form :method :post :onsubmit (remote-function continuation :form t))
	  (:table
	   (:tr
	    ((:td :align :right) "From this color:")
	    (:td ((:input :name "color1" :class "color" :value "4A6EFF")) :br))
	   (:tr
	    ((:td :align :right) "To this color:")
	    (:td ((:input :name "color2" :class "color" :value "FFAA54")) :br))
	   (:tr
	    ((:td :align :right) "# swatches:")
	    (:td ((:input :name "n" :value 24))))
	   (:tr 
	    (:td ((:input :type :submit :value "Interpolate"))))))
	 ((:div :id "result") (:i "Result goes here"))
	 (render-scripts
	   (:js "jscolor.init();"))))
      (tracker)
      ))))

	 
