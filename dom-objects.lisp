(in-package :wu)

(export '(html-element 
	  element-named element-render element-update
	  html-element-dom-id)
	)

(defclass* html-element ()
  ((dom-id nil))
  :initable-instance-variables
  :readable-instance-variables)

;;; should be defvar but slime has problem with that
(defparameter *dom-id-counter* 0)

(defun gen-dom-id ()
  (format nil "id~A" (incf *dom-id-counter*)))

(def-session-variable *dom-ht* (make-hash-table :test #'equal))

(defmethod* initialize-instance :after ((e html-element) &rest ignore)
            (unless dom-id
              (setf dom-id (gen-dom-id)))
            (setf (gethash dom-id *dom-ht*) e))

(defmethod* element-update ((e html-element))
  (render-update
    (:replace dom-id
             (element-render e))))

(defun element-named (dom-id)
  (gethash dom-id *dom-ht*))
