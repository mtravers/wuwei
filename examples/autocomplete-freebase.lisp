(in-package :wu)

;;; +++ this is having json version trouble, should replace all keywords with strings in MQL.

(publish :path "/mql-autocomplete-simple-demo"
	 :content-type "text/html"
	 :function 'mql-autocomplete-simple-demo)

(publish-code)

(defun mql-autocomplete-simple-demo (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:head
      (:title "Auocomplete simple demo")
      (javascript-includes "prototype.js" "effects.js" "controls.js" "wuwei.js")
      (css-includes "wuwei.css"))
     (:html 
      ((:body :id "body")
       (example-header #.(this-pathname))
       (:h3 "Autocomplete Demo")
       (:princ "This example shows the use of an autocomplete field, using Freebase as the backend.") :p
       :newline
       "Enter an author (eg \"Minsky\"): " :br
       (autocomplete-mql-field :type "/book/author"
			       :anchor-start? nil
			       :show-ids? nil
			       :input-options '(:size 60)
			       :on-selected
			       #'(lambda (value string id)
				   (declare (ignore id))
				   (render-update 
				     (:update "result" 
					      (html
					       ((:a :href (freebase-url value) :target "freebase")
						(:princ-safe string))
					       "  " (:princ-safe value)
					       :p
					       ((:table :border 1)
						(:tr
						 (:th "Book")
						 (:th "Pub Date")
						 (:th "Subjects"))
						(dolist (book-mql 
							  (cdr (assoc :works_written 
								      (car
								       (mql-read
									`(((:id . ,value)
									  (:type . "/book/author")
									  ("works_written" . (((:id . nil) 
											       ("a:name" . nil)
											       ("/book/written_work/date_of_first_publication" . nil)
											       ("/book/written_work/subjects" .
															      (((:id . nil) 
																("a:name" . nil)
																("optional" . t))))
											       )))
									  
									  )))))))
						  (html
						   (:tr
						    (:td ((:a href (freebase-url (cdr (assoc :id book-mql)))  :target "freebase")
							  (:princ-safe (cdr (assoc :|A:NAME| book-mql)))))
						    (:td (awhen (cdr (assoc :/BOOK/WRITTEN_WORK/DATE_OF_FIRST_PUBLICATION book-mql))
							   (html (:princ it))))
						    (:td (dolist (subject-mql (cdr (assoc :/BOOK/WRITTEN_WORK/SUBJECTS book-mql)))
							   (html
							    ((:a href (freebase-url (cdr (assoc :id subject-mql)))  :target "freebase")
							     (:princ-safe (cdr (assoc :|A:NAME| subject-mql)))) 
							    :br)))
						    ))))
					       
					       
					       ))
				     )))
       ((:div :id "result") (:i "result goes here"))
       (tracker)
       )))))

(defun freebase-url (id)
  (string+ "http://www.freebase.com/view" id))

(defun autocomplete-mql-field (&rest other &key anchor-start? type show-ids? &allow-other-keys)
  (apply 'auto-complete-field 
	 :completions-generator
	 #'(lambda (prefix)
	     (mapcar #'(lambda (item)
			 (cons (cdr (assoc :id item))
			       (if show-ids?
				   (string+ 
				    (cdr (assoc :|A:NAME| item))
				    (format nil " (~A)" (cdr (assoc :id item))))
				   (cdr (assoc :|A:NAME| item)))))
		     (mql-autocomplete prefix type :anchor-start? anchor-start?)))
	 (delete-keyword-args '(:anchor-start? :type :show-ids?) other)))
	 

;;; MQL machinery

(defparameter *freebase-readservice* "https://www.googleapis.com/freebase/v1/mqlread")   ; Path to mqlread service

(defvar *mql-debug* nil)

(defun mql-read (q)
  (let ((json (json:encode-json-to-string q))
	 response)
    (when *mql-debug*
      (terpri)
      (princ json))
    (setq response
	  ;; Behavior changed in cl-json 0.4.0, this changes it back.
	  (let ((json:*json-identifier-name-to-lisp* #'json:simplified-camel-case-to-lisp)) 
	    (json:decode-json-from-string 
	     (coerce-drakma-to-string
	      (drakma:http-request *freebase-readservice* :parameters `(("query" . ,json)))
	     ))))
    (aif (assocdr :error response)
	 (error "MQL error ~A" it))
    (when *mql-debug*
      (terpri)
      (print response))
    (assocdr :result response)))

;;; eg: (mql-autocomplete "Marv" "/people/person")
(defun mql-autocomplete (prefix type &key (property "name") (anchor-start? nil) (limit 10))
  (mql-read
   `(((,(string+ (string property) "~=") . ,(string+ (if anchor-start? "^" "") prefix "*"))
     ("type" . ,type)
     ("id" . nil)
     ("a:name" . nil)
     ("limit" . ,limit)
     ("sort" . "a:name")
     ))))





