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

(export '(*public-directory* public-url image-url
	  
	  javascript-include javascript-includes css-include css-includes
	  
	  render-update render-scripts
	  
	  html-string 
	  html-escape-string clean-js-string

	  publish-ajax-update publish-ajax-func ajax-continuation 
	  *ajax-request* *within-render-update*

	  unpublish
	  
	  image-tag 
	  html-list
	  select-field

	  nbsp br html-princ

	  remote-function link-to
	  link-to-remote link-to-function
	  button-to-remote button-to-function
	  checkbox-to-remote radio-to-remote

	  uploader *file-field-name*

	  async async-html

	  ))

;;; Basic web functions and infrastructure.  Stuff in this file should stand on its own (no prototype or other libraries required).

;;; Define a directory and path for public files

(defparameter *public-directory* (make-pathname
				  :directory '#.(append (pathname-directory (this-pathname)) '("public"))))

(publish-directory :destination (namestring *public-directory*)
                   :prefix "/wupub/"
                   :headers `((:expires . ,(net.aserve::universal-time-to-date (+ (get-universal-time) (* 20 60 60))))))

(defun public-url (name)
  (string+ "/wupub/" name))

(defun image-url (img)
  (public-url (string+ "images/" (string img))))

(defun coerce-url  (file-or-url)
  (if (string-prefix-equals file-or-url "http:")
      file-or-url
      (public-url file-or-url)))

(defun javascript-include (file-or-url)
  (html
   ((:script :type "text/javascript" :src (coerce-url file-or-url))) :newline ))

(defun javascript-includes (&rest files)
  (mapc #'javascript-include files))

(defun css-include (file-or-url)
  (html
   ((:link :rel "stylesheet" :type "text/css" :href (coerce-url file-or-url)))))

(defun css-includes (&rest files)
  (mapc #'css-include files))
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
  (string-replace string (string #\Newline) "<br/>"))

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
  `(html :br))

(defmacro p ()
  `(html :p))

(defmacro nl ()
  `(html
     :newline))

(defmacro html-list (var)
  `(:ul
    ,@(loop for i in (eval var)
         collecting `(:li (:princ (symbol-name ,i))))))


;;; Make a Select element 

(defun select-field (&key id name options url params selected html-options)
  #.(doc "Generate an HTML select field."
	 "If URL is given, trigger off of the mouseup event"
	 "OPTIONS is a list of (value name) pairs.")
  (html
    ((:select :if* name :name name
              :if* id :id id
;;; Fix for Safari (but it depends on Ext which we no longer use)
;;; +++ Prototype.Browser.WebKit may be equivalent but haven't tried it +++
;;;            :if* url :onmouseup (format nil "if (Ext.isSafari){~a}" (remote-function url :params (append `(:type (:raw "this.value")) params)))
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




