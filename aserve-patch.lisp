(in-package :net.aserve)

#|
Fixes a bug in portableaserve.  Modern browsers return headers like this:
 Content-type:  application/x-www-form-urlencoded; charset=utf-8
The patch below makes aserve ignore the semicolon and following text. 


;;; HAS BEEN FIXED in more recent versions of aserve
|#

(defun header-first-field (s)
  (let ((sep (position #\; s)))
    (if sep
	(subseq s 0 sep)
	s)))

(defmethod request-query ((req http-request) &key (post t) (uri t)
						  (external-format 
						   *default-aserve-external-format*))
  ;; decode if necessary and return the alist holding the
  ;; args to this url.  In the alist items the value is the 
  ;; cdr of the alist item.
  ;;
  ;; If uri is true then we look for query information in the uri
  ;; (following a question mark)
  ;; If post is true and this is a post request then we look for
  ;; query information in the body of the query.
  ;; If both are true (and this is a post) then we look both places.
  ;;
  ;;
  (let ((alist (request-query-alist req))
	(signature (cons post uri)))
    
    (if* (not (eq alist :empty))
       then (let ((given-sig (getf (request-reply-plist req) 
				   'request-query-sig)))
	      (if* (equal given-sig signature)
		 then ; same args as before, cached value is legit
		      (return-from request-query alist))))
    
    (let (res)
      (if* uri
	 then (let ((arg (uri-query (request-uri req))))
		(if* arg
		   then (setq res (form-urlencoded-to-query
				   arg
				   :external-format external-format)))))
	      
      (if* post
	 then (if* (and (eq (request-method req) :post)
			(equal (header-first-field (header-slot-value req :content-type))
			       "application/x-www-form-urlencoded")
			)
		 then (setf res
			(append res
				(form-urlencoded-to-query
				 (get-request-body req)
				 :external-format external-format)))))
      (setf (getf (request-reply-plist req) 'request-query-sig)
	signature)
      (setf (request-query-alist req) res))))

(cl-user::provides :aserve-patch)
