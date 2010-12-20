(in-package :wu)

(publish :path "/mql-autocomplete-demo"
	 :function 'mql-autocomplete-demo)

;;; the page
(defun mql-autocomplete-demo (req ent)
  (with-http-response-and-body (req ent)
    (flet ((render-instance-chooser (type human-name)
	     (html ((:div :id "instance_c")
		    (autocomplete-mql-field :type type
					    :on-selected
					    #'(lambda (value string id)
						(render-update 
						  (:update "result"
							   (html
							    (:princ-safe value)))))
					    )))))

      (html
       (:head
	(javascript-includes "prototype.js" "effects.js" "controls.js" "wuwei.js")
	(css-include "wuwei.css"))
       (:html 
	((:body :id "body")
	 :newline
	 (:table
	  (:tr
	   ((:td :align :right) "Type")
	   (:td
	    (autocomplete-mql-field :type "/type/type"
				    :anchor-start? t
				    :show-ids? t
				    :on-selected
				 #'(lambda (value string id)
				     (print `(autocomplete-finish ,value ,string ,id))
				     (render-update 
				       (:update "typeid" (html (:princ-safe value)))
				       (:replace "instance_c"
						 (render-instance-chooser value string))))))
	   (:td ((:span :id "typeid"))))
	  (:tr
	   (:td "Choose an instance")
	   (:td (render-instance-chooser "/people/person" "Person"))
	   (:td ((:span :id "result")))
	 ))))))))			     


#|	   (:td "or " (link-to-remote "click to see all" (ajax-continuation ()
							   (render-update
							     (:update :instances
								      (html 
								       
								       
							   "(up to 100)"
	   )

|#

;;; the field
(defun autocomplete-mql-field (&rest other &key anchor-start? type show-ids? &allow-other-keys)
  (apply 'auto-complete-field 
	 :completions-url (ajax-continuation (:args (prefix) :keep t :name "mql_completions")
			    (html
			     (:ul
			      (dolist (item (mql-autocomplete prefix type :anchor-start? anchor-start?))
				(html
				 ((:li :id (cdr (assoc :id item)))
				  (:princ (cdr (assoc :|A:NAME| item)))
				  (if show-ids?
				      (html (format nil " (~A)" id)))))))))
	 (delete-keyword-args '(:anchor-start? :type :show-ids?) other)))
	 

;;; MQL machinery

(defvar *freebase-host* "www.freebase.com") ; The Metaweb host
(defvar *freebase-readservice* "/api/service/mqlread")   ; Path to mqlread service

(defvar *mql-debug* nil)

(defun mql-read (q)
  (let* ((env2 `((:query . ,(list q)))) ; )   `((:query . ,q)) -- but this way we always do multiple, which is usually right
	 (json (json:encode-json-to-string env2))
	 (args (net.aserve:uriencode-string json))
	 (url (format nil "http://~A~A?query=~A" *freebase-host* *freebase-readservice* args))
	 response)
    (when *mql-debug*
      (terpri)
      (princ json))
    (setq response
	  (json:decode-json-from-string 
	   (get-url url)
	   ))
    (unless (equal "/api/status/ok" (assocdr :code response))
      (error "MQL error ~A" response))
    (when *mql-debug*
      (terpri)
      (print response))
    (assocdr :result response)))

;;; eg: (mql-autocomplete "Marv" "/people/person")
(defun mql-autocomplete (prefix type &key (property "name") (anchor-start? nil) (limit 10))
  (mql-read
   `((,(string+ (string property) "~=") . ,(string+ (if anchor-start? "^" "") prefix "*"))
     (:type . ,type)
     (:id . nil)
     ("a:name" . nil)
     (:limit . ,limit)
     (:sort . "a:name")
     )))

;;; Gets way way too much stuff
(defun mql-all-properties (id &key get-frames?)
  (let ((types
	 (assocdr 
	  :type 
	  (car (mql-read `((:id . ,id)
			   (:type . :empty-list))))))
	(result nil))
    (dolist (type types)
      (setf result 
	    (append result
		    (car 
		     (ignore-errors 	;+++ some types give errors, just ignore
		       (mql-read `((:id . ,id)
				   (:type . ,type)
				   ("*" . ,(if get-frames? '(:empty-dict) :empty-list))))))) ))
    result))

(defun mql-all-properties (id &key get-frames?)
  (let ((types
	 (assocdr 
	  :type 
	  (car (mql-read `((:id . ,id)
			   (:type . :empty-list))))))
	(result nil))
    (dolist (type types)
      (setf result 
	    (append result
		    (car 
		     (ignore-errors 	;+++ some types give errors, just ignore
		       (mql-read `((:id . ,id)
				   (:type . ,type)
				   ("*" . ,(if get-frames? '(:empty-dict) :empty-list))))))) ))
    result))

(defun mql-all-properties (id &key get-frames?)
  (let ((types
	 (assocdr 
	  :type 
	  (car (mql-read `((:id . ,id)
			   (:type . :empty-list))))))
	(result nil))
    (dolist (type types)
      (dolist (prop (car 
		     (ignore-errors 	;+++ some types give errors, just ignore
		       (mql-read `((:id . ,id)
				   (:type . ,type)
				   ("*" . ,(if get-frames? '(:empty-dict) :empty-list)))))))
	;; merge results
	(aif (find (car prop) result :key #'car)
	     (if (listp (cdr it))
		 (setf (cdr it) (nunion (cdr it) (if (listp (cdr prop)) (cdr prop) (list (cdr prop))) :test #'equal))
		 (setf (cdr it) (list (cdr it) (cdr prop))))
	     (push prop result))))
    result))



