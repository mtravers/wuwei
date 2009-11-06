(in-package :wu)

(export '(html-string 
	  javascript-include javascript-includes css-include
	  
	  render-update render-scripts
	  
	  html-escape-string clean-js-string

	  publish-ajax-udpate publish-ajax-func ajax-continuation
	  *ajax-request*

	  unpublish
	  
	  link-to-remote link-to-function
	  button-to-remote button-to-function
	  checkbox-to-remote

	  async async-html
	  ))

#|
Basic web functions and infrastructure.  Stuff in this file should stand on its own (no prototype or other libraries required).
|#

(defvar *public-directory* (make-pathname :directory (append (pathname-directory cl-user::*3rdwheel-dir*) '("public"))))

(publish-directory :destination (namestring *public-directory*)
                   :prefix "/npublic/"
                   :headers `((:expires . ,(net.aserve::universal-time-to-date (+ (get-universal-time) (* 20 60 60))))))



(defvar *upload-directory* (make-pathname :directory (append (pathname-directory cl-user::*3rdwheel-dir*) '("uploads"))))

(defvar *buffer-size* (* 1024 4))
 
(defun buffered-stream-copy (in out)
  (loop with buffer = (make-array *buffer-size* :element-type '(unsigned-byte 8))
     for n = (read-sequence buffer in)
     with len = 0
     finally (return len)
     while (and n (< 0 n)) do
       (incf len n)
       (if out
           (write-sequence buffer out :end n))))

;; --> Rely on obfuscated path for security for now
;; --> In future, produce URL with SPARQL OID and enforce real security.

(defparameter *PATH-QUERY-PARAMETER* "path")

;;; +++ needs to handler missing files more gracefully -- but that depends on the filetype
(defun serve-up-document (req ent)
  (with-http-response (req ent) ;; &key timeout check-modified format response content-type)
    (with-http-body (req ent)
      (let* ((relative-path (request-query-value *PATH-QUERY-PARAMETER* req))
             (absolute-path (absolute-upload-path relative-path))
             (response (request-reply-stream req))
             )
	(with-open-file (s absolute-path :direction :input :element-type '(unsigned-byte 8))
	  (buffered-stream-copy s response))
	))))

(publish :path "/documents"
         :function 'serve-up-document)

#|
Philosophy of this library: Things work via side effect (by using the HTML macro and associated machinery).

If you want a string, wrap the call with html-string.  For example:
(link-to (html-string (image-tag "collabrx_logo_small.png"))
         "/")

|#

(defmacro html-string (&body stuff)
  `(with-output-to-string (s)
     (let ((net.aserve::*html-stream* s))
       (net.aserve::html ,@stuff))))

(defun public-url (name)
  (string+ "/npublic/" name))

(defun generate-relative-upload-path (name)
  (format () "~d/~d/~a" (random 100) (random 100) name))

(defun absolute-upload-path (relative-path)
  (string+ (namestring *upload-directory*) relative-path))

(defun uploaded-url (name)
  (format nil "/documents?~a=~a" *PATH-QUERY-PARAMETER* name))

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

(defun image-url (img)
  (public-url (string+ "images/" img)))

(defun break-lines (string)
  (utils:string-replace string (string #\Newline) "<br/>"))

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

;; --> conditionalize to use html or javascript, depending on context.
;; Scrub the string more vigorously!
(defun html-report-error (&key error stack-trace)
  ;; Log this?
  (log-message (format nil "~%Unhandled exception caught by with-html-error-handling: ~a~%~a~%" error stack-trace))
  (html
    ((:div :class "error")
     (:b
      (:princ-safe (string+ "Error: " (princ-to-string error))
                   ))
     (if *developer-mode*
         (html
           (:pre
            (:princ-safe stack-trace))
           )
         )
     )
    ))


;;; If you want to close off html elements in case of an error, I think you need to add unwind-protects to  html-body-key-form
;;;  in /misc/downloads/cl-portable-aserve-1.2.42/aserve/htmlgen/htmlgen.cl
;;;  get-frames-list for a backtrace (but probably need a different kind of handler in that case)
(defmacro with-html-error-handling (&body body)
  `(utils:without-unwinding-restart (html-report-error)
     ,@body))

;;; convert lisp-style hyphenated string into camel case
(defun camel-case (string)
  (apply #'string+
         (mapcar #'string-capitalize
                 (string-split string #\-))))

;;; convert lisp-style hyphenated string into capitalized label
(defun labelify (string)
  (string-capitalize (substitute #\  #\- string)))

(publish-file :path "/favicon.ico"
              :content-type "image/x-icon"
              :file  (namestring (make-pathname :directory (append (pathname-directory cl-user::*3rdwheel-dir*) '("public" "images")) :name "favicon" :type "ico"))
              ;; (append (pathname-directory cl-user::*3rdwheel-dir*) '("public" "images" "favicon.ico"))
              )

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

(defmacro h3 (label)
  `(:h3 (:princ ,label)))

;; Make a Select element that triggers off of the mouseup event
(defun action-selector (selector-id name options url &key params selected)
  (html
    ((:select :name name
              :id selector-id
              :onmouseup (format nil "if (Ext.isSafari){~a}" (remote-function url :params (append `(:type (:raw "this.value")) params)))
              )
     (loop for (value name) in options do
          (html
            ((:option :value value
		      :onmouseup (remote-function url :params (append `(:type ,(format nil "~a" value)) params))
		      :if* (equal value selected) :selected "selected"
		      )
             ;; --> this should persist in a session variable
             (:princ-safe name) :newline)))
     )))

#|
(defmacro action-selector (selector-id name options url &optional button)
  `(html
     ((:select :name ,name
             :id ,selector-id
             :onmouseup (format nil "if (Ext.isSafari){~a}" (remote-function ,url :params `(:type (:raw "this.value"))))
             )
      ,@(loop for (value name) in (,options)
          collecting `((:option :value ,value :onmouseup ,(remote-function ,url :params `(:type ,(format nil "~a" value))))
                       ;; --> this should persist in a session variable
                       (:princ ,name) :newline))
     )))
|#




