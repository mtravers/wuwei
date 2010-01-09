(in-package :wu)

(export '(*public-directory* public-url image-url
	  
	  javascript-include javascript-includes css-include
	  
	  render-update render-scripts
	  
	  html-string 
	  html-escape-string clean-js-string

	  publish-ajax-update publish-ajax-func ajax-continuation 
	  *ajax-request* *within-render-update*

	  unpublish
	  
	  image-tag 

	  nbsp br html-princ

	  remote-function link-to
	  link-to-remote link-to-function
	  button-to-remote button-to-function
	  checkbox-to-remote

	  async async-html

	  html-list
	  action-selector

	  ))

;;; Basic web functions and infrastructure.  Stuff in this file should stand on its own (no prototype or other libraries required).

;;; Define a directory and path for public files

(defvar *public-directory* (make-pathname :directory (append (pathname-directory cl-user::*3rdwheel-dir*) '("public"))))

(publish-directory :destination (namestring *public-directory*)
                   :prefix "/npublic/"
                   :headers `((:expires . ,(net.aserve::universal-time-to-date (+ (get-universal-time) (* 20 60 60))))))

(defun public-url (name)
  (string+ "/npublic/" name))

(defun image-url (img)
  (public-url (string+ "images/" img)))

(defun coerce-url  (file-or-url)
  (if (string-prefix-equals file-or-url "http:")
      file-or-url
      (public-url file-or-url)))

(defun javascript-include (file-or-url)
    (html
     ((:script :type "text/javascript" :src (coerce-url file-or-url))) :newline ))

(defun javascript-includes (&rest files)
  (dolist (file files)
    (javascript-include file)))

(defun css-include (file-or-url)
  (html
   ((:link :rel "stylesheet" :type "text/css" :href (coerce-url file-or-url)))))


#|
Philosophy of this library: Things work via side effect (by using the HTML macro and associated machinery).

If you want a string, wrap the call with html-string.  For example:
(link-to (html-string (image-tag "logo_small.png"))
         "/")
|#

(defmacro html-string (&body stuff)
  `(with-output-to-string (s)
     (let ((*html-stream* s))
       (html ,@stuff))))

(defmacro maybe-to-string (to-string? &body body)
  `(if ,to-string?
       (html-string ,@body)
       (progn ,@body)))

(defun link-to (text url &key target)
  (html ((:a :href url :if* target :target target)
         ;; :princ rather than :princ-safe to allow html embedding
         (:princ text))))

(defun image-tag (img &key alt border width height to-string?)
  (maybe-to-string to-string?
                   (html ((:img :src (image-url img)
                                :if* width :width width
                                :if* height :height height
                                :if* border :border border
                                :if* alt :alt alt
                                :if* alt :title alt
                                )))))

(defun break-lines (string)
  (utils:string-replace string (string #\Newline) "<br/>"))


;;; convert lisp-style hyphenated string into camel case
(defun camel-case (string)
  (apply #'string+
         (mapcar #'string-capitalize
                 (string-split string #\-))))

;;; convert lisp-style hyphenated string into capitalized label
(defun labelify (string)
  (string-capitalize (substitute #\  #\- string)))

(defmacro html-princ (text)
  `(html
     (:princ-safe ,text)))

(defmacro nbsp ()
  `(html
     (:princ "&nbsp;")))

(defmacro br ()
  `(html
     (:princ "<br>")))

(defmacro nl ()
  `(html
     :newline))

#|
+++ how about generating functions for all suitable HTML commands?

(defmacro h3 (label)
  `(:h3 (:princ ,label)))
|#


(defmacro html-list (var)
  `(:ul
    ,@(loop for i in (eval var)
         collecting `(:li (:princ (symbol-name ,i))))))


;;; Make a Select element 
;;; If URL is given, trigger off of the mouseup event
;;; Options is a list of (value name) pairs.
(defun action-selector (id name options url &key params selected html-options)
  (html
    ((:select :name name
              :id id
              :if* url :onmouseup (format nil "if (Ext.isSafari){~a}" (remote-function url :params (append `(:type (:raw "this.value")) params)))
	      :do* html-options
              )
     (loop for (value name) in options do
          (html
            ((:option :value value
		      :if* url :onmouseup (remote-function url :params (append `(:type ,(format nil "~a" value)) params))
		      :if* (equal value selected) :selected "selected"
		      )
             (:princ-safe name) :newline)))
     )))





