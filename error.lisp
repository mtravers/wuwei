(in-package :wu)

;;; This stuff needs some rethinking. For now, it just gets its own file

(export '(error-box render-error clear-error
	  with-html-error-handling
	  with-json-error-handling
	  with-html-safe-error-handling
	  with-ajax-error-handler
	  ))

(defparameter *bug-report-url* "http://wuwei.org/trac")

(defun system-info ()
  "Replace with commands to get your system version info, eg by running 'hg log -l 1' in a shell")

(defun report-bug-button (&optional (info ""))
  (html
   ((:a :href (format nil "~a?description=~A" *bug-report-url* (url-encode (format nil "In ~A:~%~%~a" (system-info) info)))
	:target "error") "Report a bug")))

(defun error-box ()
  (html ((:div :id "error_box" :style "visibility:none;")))) ;invisible until replaced

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

(defun clear-error ()
  (render-update
   (:replace "error_box" (html ((:div :id "error_box"))))))

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
        (render-update
	 (:alert (princ-to-string (clean-js-string error)))
	 (:js (or extra-js ""))
; +++ This would be nice but it doesn't work, also needs to be some way to clear the error.
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

(defun need-to-login-response (req ent &optional (page "/nlogin"))
  (declare (ignore req ent))
  (html
   (render-scripts
    (:redirect page)	
    )))

(defvar *LOGGING* t)
(defvar *LOGGING-STREAM* *STANDARD-OUTPUT*)

(defun log-message (message)
  (if *LOGGING*
      (format *LOGGING-STREAM* "~a ~a~%" (net.aserve::universal-time-to-date (get-universal-time))  message))) 
