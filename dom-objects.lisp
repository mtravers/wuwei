(in-package :wu)

(export '(html-element 
	  element-named element-render element-update
	  dom-id
	  html-element-dom-id)
	)

(defclass* html-element ()
  ((dom-id nil))
  :initable-instance-variables
  :readable-instance-variables)

(defvar *dom-id-counter* 0)

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
  (or (gethash dom-id *dom-ht*)
      (error "No element named ~A" dom-id))) ;+++ may want to make error checking optional

(defgeneric element-render (dom-object)
  (:documentation "This method should do the actual HTML rendering of the object"))
  

