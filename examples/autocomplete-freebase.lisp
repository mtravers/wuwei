(in-package :wu)

(publish :path "/mql-autocomplete-demo"
	 :function 'mql-autocomplete-demo)

;;; the page
(defun mql-autocomplete-demo (req ent)
  (with-http-response-and-body (req ent)
    (html
     (:head
      (javascript-includes "prototype.js" "effects.js" "controls.js")
      (css-include "wuwei.css"))
     (:html 
      ((:body :id "body")
       "Autocomplete demo: "
       (autocomplete-mql-field :type "/people/person"))))))
			     

;;; the field
(defun autocomplete-mql-field (&rest other &key type &allow-other-keys)
  (apply 'auto-complete-field 
	 :completions-url (ajax-continuation (:args (prefix) :keep t :name "mql_completions")
			    (html
			     (:ul
			      (dolist (item (mql-autocomplete prefix type))
				(html
				 ((:li :id (cdr (assoc :id item)))
				  (:princ-safe (cdr (assoc :|A:NAME| item)))))))))
	 (delete-keyword-arg :type other)))
	 

;;; MQL

(defvar *freebase-host* "www.freebase.com") ; The Metaweb host
(defvar *freebase-readservice* "/api/service/mqlread")   ; Path to mqlread service

(defvar *mql-debug* nil)

(defun mql-read (q)
  (let* ((env2 `((:query . ,(list q)))) ; )   `((:query . ,q))
	 (json (json:encode-json-to-string env2))
	 (args (net.aserve:uriencode-string json))
	 (url (format nil "http://~A~A?query=~A" *freebase-host* *freebase-readservice* args))
	 response)
    (when *mql-debug*
      (terpri)
      (princ json))
    (setq response
	  (json:decode-json-from-string 
					;     (util:get-url url)
					;     (net.aserve.client::do-http-request url)
	   (get-url url)
	   ))
    (unless (equal "/api/status/ok" (assocdr :code response))
      (error "MQL error ~A" response))
    (when *mql-debug*
      (terpri)
      (print response))
    (assocdr :result response)))

(defun mql-autocomplete (prefix type &key (property "name") (anchor-start? t) (limit 10))
  (mql-read
   `((,(string+ (string property) "~=") . ,(string+ (if anchor-start? "^" "") prefix "*"))
     (:type . ,type)
     (:id . nil)
     ("a:name" . nil)
     (:limit . ,limit)
     )))
