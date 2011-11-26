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

(#+:ALLEGRO excl:without-redefinition-warnings #-:ALLEGRO progn

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


);excl:without-redefinition-warnings
