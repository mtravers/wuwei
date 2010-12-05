(in-package :wu)

#|
Illustrates:
- incorporating a 3rd-party Javascript library
- Ajax form handling
|#

(publish :path "/color-demo"
	 :function 'color-demo)

(defun color-demo (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:head 
      (javascript-includes "prototype.js" "effects.js" "jscolor/jscolor.js"))
     (:body
      "Choose a color: "
      ((:input :id "color" :class "color" :value "4A6EFF")))
     (render-scripts
       (:js "jscolor.init();")))))

	 
