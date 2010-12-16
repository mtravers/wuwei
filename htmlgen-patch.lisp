(in-package :net.html.generator)

#|
Extensions/patches to htmlgen.
|#

;;; Patched to support a new keyword in option list
;;; do* takes a list (key1 val1 key2 val2...) and turns the key/vals into HTML options
;;; ie (html ((:a :href "http://foo.com :do* options) ...))
(defun html-body-key-form (string-code has-inv args body)
  ;; do what's needed to handle given keywords in the args
  ;; then do the body
  (if* (and args (atom args))
     then ; single arg 
	  (return-from html-body-key-form
	    (case args
	      (:set `(write-string  ,(format nil "<~a>" string-code)
				    *html-stream*))
	      (:unset (if* has-inv
			 then `(write-string  ,(format nil "</~a>" string-code)
					      *html-stream*)))
	      (t (error "illegal arg ~s to ~s" args string-code)))))
  (if* (not (evenp (length args)))
     then (warn "arg list ~s isn't even" args))
  (if* args
     then `(progn (write-string ,(format nil "<~a" string-code)
				*html-stream*)
		  ,@(do ((xx args (cddr xx))
			 (res))
			((null xx)
			 (nreverse res))
			(case (car xx)
			  (:if* 
			   (push `(if* ,(cadr xx)
				       then (write-string 
					     ,(format nil " ~a" (caddr xx))
					     *html-stream*)
				       (prin1-safe-http-string ,(cadddr xx)))
				 res)
			   (pop xx) (pop xx))
			  (:do*
			   (push `(do ((yy ,(cadr xx) (cddr yy)))
				      ((null yy))
				    (format *html-stream* " ~a" (car yy))
				    (prin1-safe-http-string (cadr yy)))
				 res))

			  (t
			   (push `(write-string 
				   ,(format nil " ~a" (car xx))
				   *html-stream*)
				 res)
			   (push `(prin1-safe-http-string ,(cadr xx)) res))))
						    
		      
		  (write-string ">" *html-stream*)
		  ,@body
		  ,(if* (and body has-inv)
		      then `(write-string ,(format nil "</~a>" string-code)
					  *html-stream*)))
     else `(progn (write-string ,(format nil "<~a>" string-code)
				*html-stream*)
		  ,@body
		  ,(if* (and body has-inv)
		      then `(write-string ,(format nil "</~a>" string-code)
					  *html-stream*)))))
