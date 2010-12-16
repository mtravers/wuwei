(in-package :wu)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers

#|
Notes: this is sort of half-baked
- elements are responsible for rendering their dom-id themselves (in a <div> typically)
|#


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
  

