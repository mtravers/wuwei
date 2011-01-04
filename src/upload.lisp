(in-package :wu)

(export '(parse-upload-form))

;;; Call this to parse a multipart form.  

;;; Returns a list of ((<field> <value>)..., where <value> will be a pathname for the file.
;;; Not thoroughly tested, this is a very annoying piece of the web protocol

(defun parse-upload-form (req &key (pathname-maker
				    #'(lambda (name) (make-pathname :defaults (pathname "/tmp/") :name (string+ (string *session*) "_" name)))
				    ))
  (do (len (result nil))
      (())
    (multiple-value-bind (part-type field filename content-type)
	(parse-multipart-header (get-multipart-header req))
;      (format t "~%Multipart part-type=~s name=~s filename=~s content-type=~s~%" part-type field filename content-type)      
      (case part-type
	(:eof
	 (return result))
	(:file
	 (let ((pathname (funcall pathname-maker filename))
	       (element-type '(unsigned-byte 8)))
	   (with-open-file (s pathname :direction :output :if-exists :supersede :element-type element-type)
	     (setq len (slurp-part req s :element-type element-type)))
	   (push (list field pathname) result)))
	(:nofile
	 (push (list field nil) result))
	(:data
	 (push (list field 
		     (with-output-to-string (s)
		       (slurp-part req s))) 
	       result))
	(t (warn "Unknown par ttype ~A" part-type)
	   (slurp-part req))))))		

(defparameter *buffer-size* (* 4 1024))

(defun slurp-part (req &optional stream &key (element-type '(unsigned-byte 8)))
  (loop with buffer = (make-array *buffer-size* :element-type element-type)
     for n = (get-multipart-sequence req buffer)
     with len = 0
     finally (return len)
     while n do
       (incf len n)
       (if stream
           (write-sequence buffer stream :end n))))

