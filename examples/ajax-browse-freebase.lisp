(in-package :wu)

(publish :path "/fb-browse" 
	 :content-type "text/html"
	 :function 'fb-browse-page)

(setq *default-no-session?* t)

(defun render-frame (f)
  (html
   ((:div :id (mt:fast-string f) :style "border:1px solid gray")
    ((:span :style "background:gray;")
     (:b (:princ-safe (frame-human-name f))))
    :br
    ((:div )
     (:table
      (dolist (slot (list-limit (frame-slots f))) 100))
     (html
      (:tr
       (:td
	(:princ-safe (frame-human-name (car slot))))
       (:td
	(dolist (val (list-limit
		      (cdr slot)
		      6))
	  (html (render-value val)
		:br)))))))))

(def-cached-function frame-slots (f)
  (mql-all-properties f :get-frames? t))

(defun frame-slot-value (f slot)
  (assocdr slot (frame-slots f)))

(defun frame-human-name (f)
  (frame-slot-value f :name))

(defun framep (thing)
  (or (keywordp thing)
      (and (stringp thing)
	   (char= (char thing 0) #\/))))

(defmethod render-value  ((thing t))
  (if (framep thing)
      (render-value thing)
      (html (:princ-safe thing))))

(defmethod render-value ((f ocelot-gfp::frame))
  (let ((id (string (gensym "id"))))
    (html
     ((:span :id id)
      (link-to-remote (frame-human-name f)
		      (ajax-continuation ()
			(render-update
			  (:replace id
				    (render-frame f)))))))))

(defun list-limit (l n)
  (subseq l 0 (min n (length l))))

(defun dbrowse-page (req ent)
  (with-http-response-and-body (req ent)
    (html (:head
	   (javascript-includes "prototype.js" "effects.js" "dragdrop.js")
	   )
	  (:body
	   (:h1 "Dbrowse")
	   (render-frame "/en/marvin_minsky")))))
