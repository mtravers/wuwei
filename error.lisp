(in-package :wu)


;;; This stuff needs some rethinking. For now, it just gets its own file


(export '(error-box render-error clear-error
	  
	  with-html-error-handling
	  with-html-safe-error-handling
	  with-ajax-error-handler
	  ))

(defun error-box ()
  (html ((:div :id "error_box"))))

(defun render-error (msg &key stack-trace user-error?)
  (render-update
    (:replace "error_box"
              (html
                ((:div :class (if user-error? "uerror" "error") :id "error_box") ;!!! have to keep this id or later errors won't work
                 (:princ-safe msg)
		 (unless user-error?
		   (html
;+++		    (report-bug-button stack-trace)
		    ((:a :onclick "toggle_visibility('error_box_stack_trace');") "&nbsp;Show stack&nbsp;")
		    ((:div :class "error" :id "error_box_stack_trace")
		     ((:script :type "text/javascript")
		      "make_invisible('error_box_stack_trace');"
		      )
		     (:pre
		      (:princ-safe stack-trace))
		     ))))))))

(defun clear-error ()
  (render-update
   (:update "error_box" "")))

(defun compose-error-message (path &key error stack-trace)
  (let ((message (format nil "Lisp error while servicing ~a: ~A~:[~;~a~]" path error *developer-mode* (clean-js-string stack-trace))))
    (log-message message)
    ;;; This doesn't work; the header is already generated and sent.
    ;(setf (request-reply-code *ajax-request*) 400)
    (if *multipart-request*
        (html
          (:princ (json:encode-json-to-string `((failure . true)
                                                ;;(success . false)
                                                (message . ,(clean-js-string message))))))
        (render-update
	 (:alert (princ-to-string error))
; +++ This would be nice but it doesn't work, also needs to be some way to clear error.
;	 (:show "error_box")
;	 (:update "error_box"  (princ-to-string error)))
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

;;; If you want to close off html elements in case of an error, I think you need to add unwind-protects to  html-body-key-form
;;;  in /misc/downloads/cl-portable-aserve-1.2.42/aserve/htmlgen/htmlgen.cl
;;;  get-frames-list for a backtrace (but probably need a different kind of handler in that case)
(defmacro with-html-error-handling (&body body)
  `(utils:without-unwinding-restart (html-report-error)
     ,@body))

;;; Another method: do all generation to a string; if an error occurs catch it and make a error block instaned
(defmacro with-html-safe-error-handling (&body body)
  `(let ((result
	  (handler-case 
	      (html-string ,@body)
	    (error (e)
	      (html-string
	       (html-report-error :error e)
	       )))))
     (write-string result *html-stream*)))

;;; I don't know what this thinks its doing
(defmacro with-ajax-error-handler (name &body body)
  `(without-unwinding-restart (compose-error-message ,name)
    ,@body
    ))

(defun need-to-login-response (req ent)
  (declare (ignore req ent))
  (html
   (render-scripts
    (:redirect "/nlogin")		;+++ parameterize
    )))

(defvar *LOGGING* t)
(defvar *LOGGING-STREAM* *STANDARD-OUTPUT*)

(defun log-message (message)
  (if *LOGGING*
      (format *LOGGING-STREAM* "~a ~a~%" (net.aserve::universal-time-to-date (get-universal-time))  message))) ; +++ (user-string)
