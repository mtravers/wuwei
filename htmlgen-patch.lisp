(in-package :net.html.generator)

#|
Patched because schar was causing problems
|#
(defun emit-safe (stream string)
  ;; send the string to the http response stream watching out for
  ;; special html characters and encoding them appropriately
  (do* ((i 0 (1+ i))
	(start i)
	(end (length string)))
      ((>= i end)
       (if* (< start i)
	  then  (write-html-string string
                                   stream
                                   :start start
                                   :end i)))
	 
      
    (let ((ch (char string i))		;was schar
	  (cvt ))
      (if* (eql ch #\<)
	 then (setq cvt "&lt;")
       elseif (eq ch #\>)
	 then (setq cvt "&gt;")
       elseif (eq ch #\&)
	 then (setq cvt "&amp;")
       elseif (eq ch #\")
	 then (setq cvt "&quot;"))
      (if* cvt
	 then ; must do a conversion, emit previous chars first
		
	      (if* (< start i)
		 then  (write-sequence string
				       stream
				       :start start
				       :end i))
	      (write-string cvt stream)
		
	      (setq start (1+ i))))))
