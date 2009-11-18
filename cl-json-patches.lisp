(in-package :json)

#|
Patches to lib/cl-json/src/encoder.lisp

NOTE: as it happens these are patched in our source, so this file is not needed or loaded.
But it might be useful if we get a new version of cl-json, so kept here for reference.

ought to make these work in the decoding direction

New features:
:empty-list
:empty-dict
(:raw "function(foo) ...")
  produces unquoted string for javascript

|#

(defmethod encode-json ((s (eql :empty-dict)) stream)
  (write-string "{}" stream))

(defmethod encode-json ((s (eql :empty-list)) stream)
  (write-string "[]" stream))

(defmethod encode-json ((s list) stream)
;  (print `(encode list ,s))
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

