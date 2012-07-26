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
Tools for rendering Ajax updates, based on the similar functionality found in Rails see:
  http://api.rubyonrails.org/classes/ActionView/Helpers/PrototypeHelper/JavaScriptGenerator/GeneratorMethods.html

Note: to use these, you need to have prototype and (for some operations) scriptaculous.
Here's an easy way to do include them:
    (javascript-includes "prototype.js" "effects.js" "dragdrop.js")

For examples, see the test and examples directories.

Available render-update operations, hopefully mostly self-explanatory.  See the Rails and Prototype documentation for details.

(:insert <position> <html>)
(:update <id> <html>)
(:replace <id> <html>)
(:remove <id>)
(:hide <id>)
(:show <id>)
(:toggle <id>)
;; +++ :add-class :remove-class, addClassName(<elt>, <name>)...

(:draggable <id> <options>)
(:drop-target <id> <options>)

(:js <javascript>)

(:redirect <url>)
(:reload)
(:delay <seconds> <other-forms>)
(:alert <msg>)

(:visual_effect <effect-name> <id> <options>)

Here's an example of combining render-update operations:

(defun render-animated-delete (id &optional (factor 1))
  (render-update
    (:visual-effect :blind-up id :duration (* factor 0.25))
    (:visual-effect :fade id :duration (* factor 0.5))
    (:delay (* factor .5)
	    (:remove id))))

|#

;;; Define an update method handler

(defmacro define-render-update (type args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ,type :renderer)
           (named-lambda ,type ,args
                         ,@body))))

(define-render-update :update (elt htmlspec)
  `(let ((html
          (with-output-to-string (s)
            (let ((*html-stream* s))
              (html ,htmlspec)))))
     (format *html-stream* "~%Element.update('~A', ~A);" ,elt (json:encode-json-to-string html))))

(define-render-update :replace (elt htmlspec)
  `(let ((html
          (with-output-to-string (s)
            (let ((*html-stream* s))
              (html ,htmlspec)))))
     ;; This works better with FireFox, not sure why...
     ;; actually it doesn't, it leaves an extra DIV, so this is not right...
;     (format *html-stream* "~%document.getElementById('~a').innerHTML=~A;" ,elt (json:encode-json-to-string html))
     (format *html-stream* "~%Element.replace('~A', ~A);" ,elt (json:encode-json-to-string html))
;;; another alternate form
;     (format *html-stream* "~%$('~A').replace(~A);" ,elt (json:encode-json-to-string html))
     ))

(define-render-update :insert (position elt htmlspec)
  `(let ((html
          (with-output-to-string (s)
            (let ((*html-stream* s))
              (html ,htmlspec)))))
     (format *html-stream* "~%Element.insert('~A',{~A: ~A});" ,elt (string-downcase (string ,position)) (json:encode-json-to-string html))))

(defmacro define-render-element-operation (keyword &optional (func (string-downcase (string keyword))))
  `(define-render-update ,keyword (elt)
     `(format *html-stream* "~%Element.~A('~A');" ,,func ,elt)))

(define-render-element-operation :hide)
(define-render-element-operation :show)
(define-render-element-operation :toggle)
(define-render-element-operation :remove)

(defun escape-single-quotes (string)
  (mt:string-replace string "'" "\\'"))

;;; pretty simple!
(define-render-update :js (string)
  `(progn (terpri *html-stream*)
	  (awhen ,string (write-string it *html-stream*))))

;;; A script that gets inserted after the normal updates
(define-render-update :post-js (string)
  (render-script-later string))

(define-render-update :redirect (url)
  `(format *html-stream* "~%window.location.replace('~A');" ,url)) ;heard that this is better...

(define-render-update :popup (url &optional (name "_popup") (width 300) (height 300))
  `(format *html-stream* "~%window.open('~A', '~A', 'width=~A,height=~A');" ,url ,name ,width ,height))  

(define-render-update :reload ()
  `(format *html-stream* "~%window.location.reload();"))

(define-render-update :delay (seconds &rest other-forms)
  `(progn
     (format *html-stream* "setTimeout(function() {")
     ,@(mapcar #'(lambda (clause)
                   (apply (or (get (car clause) :renderer)
                              (error "Don't know how to do operation ~A" (car clause)))
                          (cdr clause)))
               other-forms)
     (format *html-stream*  "}, ~A)" (* ,seconds 1000))))

(define-render-update :visual-effect (effect elt &rest options)
  `(format *html-stream* "~%Effect.~A('~A', ~A);" (camel-case (string ,effect)) ,elt (json-options (list ,@options))))

(define-render-update :alert (message)
  `(format *html-stream* "~%alert('~A');" (escape-single-quotes ,message)))

;;; dynamically bound to allow some things to change their behaviors.
(defvar *within-render-update* nil)

;;; Mechanism for including js in HTML that might be an Ajax update or not.
(defvar *render-update-scripts* nil)

(defvar *render-debugging* nil)

(defmacro render-debug (msg)
  `(when *render-debugging*
     (format t "~%render-debug: ~A" ,msg)))

;;; Wrap this around anything that does javascript updating.  
;;; Saner version (duplicates body, might want to fix that with a closure or something +++)
(defmacro with-render-update (&body body)
  `(if *within-render-update*
       (progn ,@body)
       (let ((*render-update-scripts* nil)
	     (*within-render-update* t))
	 ,@body
	 (render-update-scripts))))

(defun render-script-later (script)
  (push-end script *render-update-scripts*))

;;; Render
(defun render-update-scripts ()
  (dolist (script *render-update-scripts*)
    (render-debug (list 'script-out script))
    (write-string script *html-stream*)))

(defmacro render-update (&body clauses)
  `(with-render-update
     ,@(mapcar #'(lambda (clause)
		   (apply (or (get (car clause) :renderer)
			      (error "Don't know how to do operation ~A" (car clause)))
			  (cdr clause)))
	       clauses)))


;;; Like render-update, but for use within HTML blocks.
;;; Will either render scripts in script element as part of a page, or (if done inside an render-update) collect them for
;;; appending to the update.
(defmacro render-scripts (&body clauses)
  `(if *within-render-update*
       (render-script-later (html-string
			      (render-update ,@clauses)))
       (html ((:script :type "text/javascript")
	      (render-debug "rendering <script> elt")
              (render-update ,@clauses)))
       ))

(defun html-escape-string (string)
  (with-output-to-string (stream)
    (net.html.generator::emit-safe stream string)))

;; Amazingly, FireFox will generate close tags for lisp objects
;; printed in pointy brackets, but then we *are* lying about the
;; content type.
;; This is not real JSON escaping....also not very efficient
(defun clean-upload-js-string (string)
  (string-replace 
   (string-replace string ">" "&gt;")
   "<" "&lt;")
  )

;;; Not very efficient
(defun clean-js-string (string)
  (string-replace
   (string-replace
    (string-replace string (string #\Newline) "\\n")
    "\"" "\\\"")
   "'" "\\'"))


(defvar *multipart-request*)
(defvar *ajax-request* nil)

;; If the client performs a file upload, an HTML form is used and a page of type text/html must be returned
;;; +++ why is this a macro?
(defmacro multipart? (req)
  `(let ((header (header-slot-value ,req :content-type)))
     (and header
          (string= (subseq header 0 (min (length header) (length "multipart/form-data")))
                   "multipart/form-data"))))


;;; :SESSION is NIL if no session management, T for session management, or the name of login
;;; handling procedure if login is required.  The procedure takes req and ent and is responsible for
;;; redirecting to a login page. 
(defmacro publish-ajax-update (path-or-options &body body)
  (let* ((path (if (listp path-or-options)
		   (findprop :path path-or-options)
		   path-or-options))
	 (options (if (listp path-or-options) path-or-options))
	 (content-type (and (listp path-or-options) (findprop :content-type path-or-options)))
	 (session (aif (and (listp path-or-options) (member :session path-or-options))
		    (cadr it)
		    t))		;defaults to t
	 (login-handler (aif (and (listp path-or-options) (member :login-handler path-or-options))
			  (cadr it))))
    (setf options (delete-keyword-args '(:path :session) options))
    `(publish-temporarily ,path
              :function #'(lambda (req ent)
			  (let* ((*multipart-request* (multipart? req))
				 (*ajax-request* req)
				 ;; +++ not sure what this condition on *multipart-request* was for, seems wrong
				 (content-type ,(or content-type `(if *multipart-request* "text/html" "text/javascript"))))
			    (,@(if session 
				   `(with-session (req ent ,@(if login-handler `(:login-handler ,login-handler))))
				   '(progn))
			       (with-http-response-and-body (req ent :content-type content-type)
				 (with-ajax-error-handler (,path)
				   (with-render-update
				     ,@body
				     )))))
			  )
	      ,@options
	      )))

(defmacro publish-ajax-func (path-or-options args &rest body)
  `(publish-ajax-update ,path-or-options
                        (let (,@(mapcar #'(lambda (arg)
                                            `(,arg (request-query-value ',(smart-string (string arg)) req)))
                                        args))
                          ,@body)))

(defvar *ajax-counter* 0)

(defmacro ajax-continuation ((&key args keep (content-type "text/javascript") (session nil session-spec?) name login-handler timeout) &body body)
  `(let ((fname (string+ "/ajax/" ,(or name "g") "/" (fast-string (incf *ajax-counter*)))))
     (publish-ajax-func (:path fname 
			       ,@(if content-type `(:content-type ,content-type))
			       ,@(if session-spec? `(:session ,session))
			       ,@(if login-handler `(:login-handler ,login-handler))
			       ,@(if timeout `(:timeout ,timeout)))
			,args
                        ,@body
                        ,(unless keep
                               '(unpublish-path fname)))
     fname))



;;; Inexplicably not in aserve 
;;; Later versions of aserve have a non-functional function called unpublish, so we give this a different name
(defun unpublish-path (path)
  (net.aserve::unpublish-entity (net.aserve::find-locator :exact *wserver*) path nil nil))

(defun publish-temporarily (path &rest args)
  (apply 'publish :path path args)
  (set-responder-timeout path))

;; could keep this sorted I supposed
(defvar *responder-timeouts* nil)

(defun set-responder-timeout (path &optional (time (+ (now) *default-responder-timeout*)))
  (push (list time path) *responder-timeouts*))

(publish-prefix :prefix "/ajax/"
		:function 'ajax-timeout)

(defun ajax-timeout (req ent)
  (with-http-response-and-body (req ent :content-type "text/javascript")
    (render-update
      (:alert "Command expired.  Try reloading the page"))))

(defun do-responder-timeouts ()
  (let* ((now (get-universal-time))
	 (expired (filter #'(lambda (item)
			      (< (car item) now))
			  *responder-timeouts*)))
    (dolist (item expired)
      (unpublish-path (cadr item)))
    (setf *responder-timeouts* (nset-difference *responder-timeouts* expired))))

#-:SBCL
(eval-when (:load-toplevel :execute)
  (in-background "Responder timeout"
                 (loop
                    (sleep (floor *default-responder-timeout* 2))
                    (do-responder-timeouts))))


;;; Drag/drop

;;; See here for description of options: http://wiki.github.com/madrobby/scriptaculous/draggable
;;; to make this useful, need patched cl-json that can do :raw strings.

(define-render-update :draggable (elt &rest options)
  `(format *html-stream* "~%new Draggable('~A', ~A);"
           ,elt
           (json-options (list ,@options))))

;;; Define an entire CSS class as draggable.
;;; uses a local extension to scriptaculous.  Does not apply to elements or classes added after the fact, a serious limitation.
(define-render-update :draggable-class (class &rest options)
  `(format *html-stream* "~%Draggable.addClass('~A', null, ~A);"
           ,class
           (json-options (list ,@options))))

;;; options are as specified by scriptaculous, see http://wiki.github.com/madrobby/scriptaculous/droppables
;;; Particularly useful:
;;;   :accept <css class> or list of classes -- specifies what can be dropped here.
;;;   :|onDrop| `(:raw "function (elt) {...}") -- call the function when a drop occurs
(define-render-update :drop-target (elt &rest options)
  `(format *html-stream* "~%Droppables.add('~A', ~A);"
           ,elt
           (json-options (list ,@options))))

;;; turn keywords (:k1 v1 :k2 v2 ...) into CL-JSON
(defun json-options-transform (options)
  (do ((rest options (cddr rest))
       (result nil))
      ((null rest)
       (nreverse result))
    (push (cons (smart-string (car rest)) (cadr rest))
          result)))

;; as above but produce JSON string
(defun json-options (options)
  (json:encode-json-to-string
   (json-options-transform options)))

;;; UPCASE turned to downcase, mixed case is left alone.  Needs a better name
(defun smart-string (k)
  (let ((s (string k)))
    (if (string-upcase? s)
        (string-downcase s)
        s)))

(defun string-upcase? (s)
  (every #'(lambda (c)
             (or (not (alpha-char-p c))
                 (upper-case-p c)))
         s))

;;; Equivalent of link_to_remote etc.  Could take more options.
;;; We can now deal with arbitrary html-options, so regularize the calling sequence of these...

;;; default is :princ rather than :princ-safe to allow image tags in text.  
;;; Should be rethought, maybe this should be a macro that wraps arbitrary html gen.
(defun link-to-function (text js &key html-options safe?)
  (html
   ((:a :href "#" :onclick js :do* html-options)
    (if safe?
	(html (:princ-safe text))
	(html (:princ text))))) )

(defun button-to-function (text js &key html-options)
  (html
   ((:input :type "button" :value text :onclick js :do* html-options))))

(defun link-to-remote (text url &rest remote-function-options &key html-options &allow-other-keys)
  (link-to-function text (apply #'remote-function url (delete-keyword-args '(:html-options) remote-function-options))
		    :html-options html-options))

(defun button-to-remote (text url &rest remote-function-options &key html-options &allow-other-keys)
  (button-to-function text (apply #'remote-function url (delete-keyword-args '(:html-options) remote-function-options))
		      :html-options html-options))

(defun checkbox-to-function (text js &key html-options)
  (html
   ((:input :type "checkbox" :onclick js :do* html-options))
    (:princ "&nbsp;")
    (:princ-safe (or text ""))
   ))
    
;;; +++ SBCL sniffs at having &optional and &key in the same arglist, and maybe it should be changed
;;; +++ copy params, class keyword functionality to link-to-remote, button-to-remote, etc
(defun checkbox-to-remote (text url &optional checked? &rest remote-function-options &key params (id (string (gensym "check"))) class html-options &allow-other-keys)
  (checkbox-to-function
   text 
   (apply #'remote-function url :in-function? nil :params `(:checked (:raw ,(format nil "$('~A').checked" id)) ,@params) (delete-keyword-args '(:html-options :id :class) remote-function-options))
   :html-options 
   `(:id ,id ,@(if class `(:class ,class)) ,@(if checked? '(:checked "true")) ,@html-options)))

(defun radio-to-remote (text url &optional checked? &rest remote-function-options &key html-options &allow-other-keys)
  (html
   ((:input :type :radio :if* checked? :checked "true" :onclick (apply #'remote-function url (delete-keyword-args '(:html-options) remote-function-options))
	    :do* html-options)
    (:princ "&nbsp;")
    (:princ-safe (or text ""))
    )))

(defun goto-url-function (url)
  (format nil "~%window.location.href = '~A';" url))

(defvar *uploader-html*
  (concatenate 'string
               "<div id='~a'></div>"
               "<script TYPE='text/javascript'>make_uploader('~a', '~a', '~a', ~a);</script>"
               ))

(defparameter *file-field-name* "Data")

(defun uploader (id url &optional isDrugrank)
  (format nil *uploader-html* id id url *file-field-name* (if isDrugrank "true" "false"))
  )

(defun remote-function (url &key form params (in-function? t) confirm before after spinner
                        success failure complete eval-scripts? stop-propagation?
			updater? periodic?)
  #.(doc
     "Generate a remote function (javascript Ajax call)"
     " ex: (remote-function \"/new-chunk\" :params `(:user ,user :type (:raw ,(format nil \"$(~A).value\" selector-id))))"
     " returns:"
     "  new Ajax.Request('/new-chunk', {\"asynchronous\":true,\"parameters\":{\"user\":\"mt\",\"type\":$(selector23).value}}); return false;"
     ":form      If t, serialize the surrounding form; if a string serialise the form with that name; else use params"
     ":params    List of (:key1 value1 ...), ignored if :form is t"
     ":confirm   Ask user for confirmation first (value is the message)"
     ":complete  Javascript to execute when action completes"
     ":success   as :complete, but on success only"
     ":failure   as :complete, but on failure only"
     ":before    Javascript to run before the Ajax request"
     ":after     Javascript to run after the Ajax request"
     ":spinner   The ID of an elt, a spinner will be inserted after the elt before the Ajax request and removed when completed"
     ":in-function?  "
     ":eval-scripts?  "   ;;; +++ only valid for Ajax.Update object?
     ":stop-propagation?   Stop propagation of events to parents. Forces :in-function? to be nil"
     ":updater?  Make an Ajax.Updater object rather than an Ajax.Request; value is dom id of item to be updated"
     ":periodic? Make an Ajax.PeriodicalUpdater, updater? must be non-nil"
     )
  (when stop-propagation?
    (setq in-function? nil))		;incompatible, at least for now.
  (when spinner
    (let ((spin-js (format nil "add_spinner('~A');" spinner))
          (nospin-js (format nil "remove_spinner('~A');" spinner)))
      (setf before (if before
                       (string+ before spin-js)
                       spin-js))
      (setf complete (if complete
                         (string+ nospin-js complete)
                         nospin-js))))
  (let* ((options
	  `(:asynchronous t
			  :parameters ,(if form
					   `(:raw ,(format nil "Form.serialize(~A)"
							   (if (stringp form)
							       (format nil "document.getElementById('~A')" form)
							       "this")))
					   (json-options-transform params))
			  ,@(if complete `("onComplete" (:raw ,(format nil "function(request){~A}" complete))))
			  ,@(if success `("onSuccess" (:raw ,(format nil "function(request){~A}" success))))
			  ,@(if failure `("onFailure" (:raw ,(format nil "function(request){~A}" failure))))
			  ,@(if eval-scripts? `("evalScripts" t))
			  ))
	(result
	 (cond (periodic?
		(assert updater?)
		(setf options (append `(:frequency ,periodic?) options))
		(format nil "new Ajax.PeriodicalUpdater('~A', '~A', ~A);" updater? url (json-options options)))
	       (updater?
		(format nil "new Ajax.Updater('~A', '~A', ~A);" updater? url (json-options options)))
	       (t
		(format nil "new Ajax.Request('~A', ~A);" url (json-options options))))))
    (when before (setf result (string+ before result)))
    (when after (setf result (string+ result after)))
    (when confirm (setf result (format nil "if (confirm('~A')) { ~A };" confirm result)))
    (when stop-propagation?
      (setf result (format nil "~A Event.stop(event);"  result)))
    (when in-function?
      (setf result (string+ result "return false;")))
    result))






