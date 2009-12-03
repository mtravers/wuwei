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
