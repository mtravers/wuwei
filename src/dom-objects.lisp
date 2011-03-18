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
Facility for defining UI objects that get rendered in HTML

Notes: 
- this is sort of half-baked, but usable
- elements are responsible for rendering their dom-id themselves (in a <div> typically)
|#

(export '(html-element 
	  element-named element-render element-update
	  dom-id parent			;+++ exporting slot names so they can be used in with-slots.  Probably no good, use methods instead.
	  html-element-dom-id
	  
	  paging-mixin display-base display-list total-size render-paging-controls
	  page-size current-page

	  flash-box flash-message
	  ))

(defclass* html-element ()
  ((dom-id nil)
   (parent))
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
      (error "No element named ~A" dom-id))) 

(defgeneric element-render (dom-object)
  (:documentation "This method should do the actual HTML rendering of the object"))
  
;;; Paging


(defclass paging-mixin (html-element)
  ((page-size :initarg :page-size :initform 25)
   (current-page :initform 0 :accessor current-page) ;page number or NIL to show all
   (show-all? :initform nil :initarg :show-all?))) ;T to generate a show-all link
 
;;; Returns a count of the total size of the paged set
(defgeneric total-size (paged-element))
;;; Returns the current elements to display based on current-page
(defgeneric display-list (paged-element))

(defmethod display-base ((object paging-mixin))
  (with-slots (page-size current-page) object
    (if current-page
	(* page-size current-page)
	0)))

(defmethod total-pages ((object paging-mixin))
  (with-slots (page-size) object
    (ceiling (total-size object) page-size)))

;;; Kind of wasteful to make a separate continuation for each page? ++
;;; Also needs to trim list down 
;;; should be customizable or use css classes +++
(defmethod render-paging-controls ((object paging-mixin))
  (with-slots (page-size current-page show-all?) object
    (let ((total-pages (total-pages object)))
      (when (> total-pages 1)
	(flet ((page-link (i &optional (label (princ-to-string (1+ i))))
		 (html
		   (if (eql i current-page)
		       (html (:b (:princ label)))
		       (link-to-remote label
				       (ajax-continuation ()
					 (setf current-page i)
					 (element-update object))))
		   (nbsp))
		 ))
	  (unless (or (not current-page) (zerop current-page))
	    (page-link (- current-page 1) "Prev"))
	  (nbsp)
	  (if (< total-pages 16)	
	      (dotimes (i total-pages)
		(page-link i))
	      ;; Too many pages, elide some
	      (let ((pages (sort (mt:union* (list (mt:integers 0 2)
						  (when current-page
						    (mt:integers (max 0 (- current-page 2))
								 (min (1- total-pages) (+ current-page 2))))
						  (mt:integers (- total-pages 3) (1- total-pages))))
				 #'<))
		    (last -1))
		(dolist (i pages)
		  (unless (= last (- i 1))
		    (html (:princ "...")))
		  (page-link i)
		  (setq last i))))
	  (unless (or (not current-page) (= current-page (1- total-pages)))
	    (page-link (+ current-page 1) "Next"))
	  (when show-all?
	    (page-link nil "Show all"))
	  )))))


;;; Paging from a fixed list

(defclass list-paging-mixin (paging-mixin)
  ((list :initarg :list)))

(defmethod total-size ((elt list-paging-mixin))
  (with-slots (list) elt
    (length list)))

(defmethod display-list ((object list-paging-mixin))
  (with-slots (page-size current-page list) object
    (if current-page
	(mt:subseq-safe list (* page-size current-page) (* page-size (1+ current-page)))
	list)))


;;; Flash

(def-session-variable *flash-messages* nil)

(defun flash-message (msg)
  (mt:push-end msg *flash-messages*))

(defclass flash-box (html-element)
  ()
  (:default-initargs :dom-id "flash"))

(defmethod element-render ((box flash-box))
  (when *flash-messages*
    (html
     ((:div :class "flash" :id "flash")
      ((:img :src (public-url "images/ex.png") :class "ex" :onclick "Element.remove($('flash'))"))
      (dolist (msg *flash-messages*)
	(html
	 ((:div :class "flash_i")
	  (:princ-safe msg))))))
    (setf *flash-messages* nil)))




