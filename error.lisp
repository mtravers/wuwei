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

;;; Author:  Mike Travers and David Sobeck

(export '(error-box render-error clear-error
	  with-html-error-handling
	  with-json-error-handling
	  with-html-safe-error-handling
	  with-ajax-error-handler
	  ))

(defun system-info ()
  "Replace with commands to get your system version info, eg by running 'hg log -l 1' in a shell")

(defun report-bug-button (&optional (info ""))
  (html
   ((:a :href (format nil "~a?description=~A" *bug-report-url* (uriencode-string (format nil "In ~A:~%~%~a" (system-info) info)))
	:target "error") "Report a bug")))

;;; Insert an error box for use by the error handler (++ should have a clear button)
(defun error-box ()
  (html ((:div :id "error_box" :style "display:none;")))) ;invisible until replaced

;;; This isn't called anywhere  (and should :update and set invisible rather than :replace)
(defun clear-error ()
  (render-update
   (:replace "error_box" (html ((:div :id "error_box"))))))

;;; This isn't called anywhere (and should :update rather than :replace)
(defun render-error (msg &key stack-trace user-error?)
  (render-update
    (:replace "error_box"
              (html
                ((:div :class (if user-error? "uerror" "error") :id "error_box") ;!!! have to keep this id or later errors won't work
                 (:princ-safe msg)
		 (unless user-error?
		   (html
		    (report-bug-button stack-trace)
		    ((:a :onclick "toggle_visibility('error_box_stack_trace');") "&nbsp;Show stack&nbsp;")
		    ((:div :id "error_box_stack_trace" :style "display:none;")  ;:class "error"
		     (:pre
		      (:princ-safe stack-trace))
		     ))))))))


;;; Set to T to use the error box rather than alert method.  
(defvar *ajax-error-box?* nil)

;;; ++ needs better name: this composes, logs, and sends it back to client
(defun compose-error-message (path &key error stack-trace extra-js)
  (let ((message (format nil "Lisp error while servicing ~a: ~A~:[~;~a~]" path error *developer-mode* stack-trace)))
    (log-message message)
    ;;; This doesn't work; the header is already generated and sent.
    ;(setf (request-reply-code *ajax-request*) 400)
    (if *multipart-request*
        (html
          (:princ (json:encode-json-to-string `((failure . true)
                                                ;;(success . false)
                                                (records ((data . ,(clean-upload-js-string message))))))))
	(let ((estring (clean-js-string (princ-to-string error))))
	  (if *ajax-error-box?*
	      (render-update
		(:update "error_box" (:princ-safe estring))
		(:show "error_box"))
	      ;; alertbox method
	      (render-update
		(:alert estring)))
	  (when extra-js
	    (render-update
	      (:js extra-js)))
	  ))))


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
     (if (and stack-trace *developer-mode*)
         (html
           (:pre
            (:princ-safe stack-trace))
           )
         )
     )
    ))

(defun create-block-for-error (&key error stack-trace)
  (html-report-error :error error :stack-trace stack-trace)
  (write-string (html-string
    (html-report-error :error error))))

;;; Another method: do all generation to a string; if an error occurs catch it and make a error block instead
(defmacro with-html-safe-error-handling (&body body)
  `(without-unwinding-restart (create-block-for-error)
     (write-string (html-string ,@body) *html-stream*)))


(defmacro with-ajax-error-handler ((name &key extra-js) &body body)
  `(without-unwinding-restart (compose-error-message ,name :extra-js ,extra-js)
    ,@body
    ))

(defun json-report-error (&key error stack-trace)
  (log-message (format nil "~%Unhandled exception caught by with-html-error-handling: ~a~%~a~%" error stack-trace))
  (html
    (:princ (json:encode-json-to-string `((failure . true)
					  (success . false)
					  (message . ,(format nil "~A" error)))))))

(defmacro with-json-error-handling (&body body)
  `(without-unwinding-restart (json-report-error)
     ,@body))

;;; If you want to close off html elements in case of an error, I think you need to add unwind-protects to  html-body-key-form
;;;  in /misc/downloads/cl-portable-aserve-1.2.42/aserve/htmlgen/htmlgen.cl
;;;  get-frames-list for a backtrace (but probably need a different kind of handler in that case)
(defmacro with-html-error-handling (&body body)
  `(without-unwinding-restart (html-report-error)
     ,@body))

(defvar *logging* t)
(defvar *logging-stream* *standard-output*)

(defun log-message (message)
  (if *logging*
      (format *logging-stream* "~a ~a~%" (net.aserve::universal-time-to-date (get-universal-time))  message))) 
