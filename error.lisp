(in-package :wu)


;;; This stuff needs some rethinking. For now, it just gets its own file


(export '(error-box render-error clear-error
	  
	  with-html-error-handling))

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
		    (report-bug-button stack-trace)
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
	;; +++ was alert, changed to this and untested.  Needs to have some way to make the error box disappear, also have different colors?
        (render-update
	 (:show "error_box")
	 (:update "error_box"  (princ-to-string error)))
        )))


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

;;; I don't know what this thinks its doing
(defmacro with-ajax-error-handler (name &body body)
  `(utils:without-unwinding-restart (compose-error-message ,name)
    ,@body
    ))




