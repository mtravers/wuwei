(in-package :json)

#|
Extensions to cl-json/src/encoder.lisp

New features, for encoding functions:
:empty-list ==> "[]"
:empty-dict ==> "{}"
(:raw "function(foo) ...") ==>
  produces unquoted string for javascript

TODO:  make these work in the decoding direction
|#

;;; changed to compatible with cl-json 0.4 -- will break in earlier versions
(defmethod encode-json ((s (eql :empty-dict)) &optional (stream *json-output*))
  (write-string "{}" stream))

(defmethod encode-json ((s (eql :empty-list)) &optional (stream *json-output*))
  (write-string "[]" stream))

(defmethod encode-json ((s list) &optional (stream *json-output*))
  (cond ((eq (car s) :raw)			
	 (write-string (cadr s) stream))
	((alistp s)
	 (encode-json-alist s stream))
	(t (call-next-method s stream))))

(defun alistp (l)
  (dolist (elt l t)
    (unless (and (consp elt)
		 (typep (car elt) '(or symbol string)))
      (return-from alistp nil))))

;;; Patch another bug in symboltojs.lisp

(defun symbol-to-js (symbol)
  (cond ((listp symbol)
        (concatenate 'string 
		     (symbol-to-js (first symbol))
                     (if (rest symbol)
			 (concatenate 'string "." (symbol-to-js (rest symbol))) "")))
        (t
         (when (symbolp symbol)
           (setf symbol (symbol-name symbol)))
         (let ((symbols (string-split symbol '(#\.))))
           (cond ((null symbols) "")
                 ((= (length symbols) 1)
                  (let (res
                        (do-not-touch nil)
                        (lowercase t)
                        (all-uppercase nil))
                    (cond ((constant-string-p symbol)
                           (setf all-uppercase t
                                 symbol (subseq symbol 1 (1- (length symbol)))))
                          ((first-uppercase-p symbol)
                           (setf lowercase nil
                                 symbol (subseq symbol 1)))
                          ((untouchable-string-p symbol)
                           (setf do-not-touch t
                                 symbol (subseq symbol 1))))
                    (flet ((reschar (c)
                             (push (cond
                                     (do-not-touch c)
                                     ((and lowercase (not all-uppercase))
                                      (char-downcase c))
                                     (t (char-upcase c)))
                                   res)
                             (setf lowercase t)))
                      (dotimes (i (length symbol))
                        (let ((c (char symbol i)))
                          (cond
                            ((eql c #\-)
                             (setf lowercase (not lowercase)))
                            ((assoc c *special-chars*)
                             (dolist (i (coerce (cdr (assoc c *special-chars*)) 'list))
                               (reschar i)))
                            (t (reschar c))))))
                    (coerce (nreverse res) 'string)))
                 (t (concatenate 'string (symbol-to-js (first symbols))
                                 "." (symbol-to-js (rest symbols)))))))))
