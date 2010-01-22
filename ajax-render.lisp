
(in-package :wu)

#|
Tools for rendering Ajax updates, based on the similar functionality found in Rails see:
  http://api.rubyonrails.org/classes/ActionView/Helpers/PrototypeHelper/JavaScriptGenerator/GeneratorMethods.html

See here for some of the magic:
  /Library/Ruby/Gems/1.8/gems/actionpack-2.3.2/lib/action_view/helpers/prototype_helper.rb

Note: to use these, you need to have prototype and (for some operations) scriptaculous.
Here's an easy way to do include them:
    (javascript-includes "prototype.js" "effects.js" "dragdrop.js")

For examples, see the test file.

Here's the list of operations from Rails.

Done:
# insert_html    :insert
# replace_html   :update     Element.update
# replace        :replace    Element.replace
# draggable      :draggable
# drop_receiving :drop-target (not fully working)
<< (raw js)      :js
# remove         :remove
# redirect_to    :redirect
# reload
# delay
# alert
# hide
# show
# toggle
# visual_effect :visual-effect

Not yet:

# assign
# call
# literal
# select
# sortable

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

;;; pretty simple!
(define-render-update :js (string)
  `(write-string ,string *html-stream*))

;;; A script that gets inserted after the normal updates (+++ experimental, not used yet)
(define-render-update :post-js (string)
  (push-end string *render-update-scripts*))

(define-render-update :redirect (url)
  `(format *html-stream* "~%window.location.href = '~A';" ,url))

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
  `(format *html-stream* "~%alert('~A');" ,message))

;;; dynamically bound to allow some things to change their behaviors.
(defvar *within-render-update* nil)

;;; Mechanism for including js in HTML that might be an Ajax update or not.
(defvar *render-update-scripts* nil)

(defun render-update-scripts ()
  (dolist (script *render-update-scripts*)
    (write-string script *html-stream*)))

;;; The main macro used to generate update code.
(defmacro render-update (&body clauses)
  `(let ((*render-update-scripts* nil))
     (let ((*within-render-update* t))
       ,@(mapcar #'(lambda (clause)
                     (apply (or (get (car clause) :renderer)
                                (error "Don't know how to do operation ~A" (car clause)))
                            (cdr clause)))
                 clauses))
     (unless *within-render-update*
       (render-update-scripts))
     ))

;;; Like render-update, but for use with HTML blocks.  Will either render scripts as part of a page, or (if done inside an Ajax update) collect them for
;;; appending to the update.
(defmacro render-scripts (&body clauses)
  `(if *within-render-update*
       (push-end (html-string
                  (render-update ,@clauses))
                 *render-update-scripts*)
       (html ((:script :type "text/javascript")
              (render-update ,@clauses)))
       ))

;;; I don't understand the above, but am temporarily too scared to mess with it.  Here's a version that is simpler and I think does
;;; the right thing, should eventually replace the above unless I'm confused ++++
(defmacro render-scripts+ (&body clauses)
  `(if *ajax-request*
       (render-update
        ,@clauses)
       (html ((:script :type "text/javascript")
              (render-update ,@clauses)))
       ))

(defun html-escape-string (string)
  (with-output-to-string (stream)
    (net.html.generator::emit-safe stream string)))

(defun clean-js-string (string)
  (string-replace
   (string-replace string (string #\Newline) "\\n")
   "\"" "\\\""))

(defvar *multipart-request*)
(defvar *ajax-request* nil)

;; If the client performs a file upload, an HTML form is used and a page of type text/html must be returned
(defmacro multipart? (req)
  `(let ((header (header-slot-value ,req :content-type)))
     (and header
          (string= (subseq header 0 (min (length header) (length "multipart/form-data")))
                   "multipart/form-data"))))

(defmacro publish-ajax-update (path-or-options &body body)
  (let ((path (if (listp path-or-options)
                  (findprop :path path-or-options)
                  path-or-options))
        (content-type (and (listp path-or-options) (findprop :content-type path-or-options)))
        (no-session? (and (listp path-or-options) (findprop :no-session? path-or-options))))
    `(publish :path ,path
              :function (named-lambda ,path (req ent)
                                      (let* ((*multipart-request* (multipart? req))
                                             (*ajax-request* req)
                                             (content-type (or ,content-type (if *multipart-request* "text/html" "text/javascript"))))
                                        (with-http-response-and-body (req ent :content-type content-type)
                                          (,@(if no-session? '(progn) '(with-session (req ent)))
                                            (with-ajax-error-handler (,path)
                                              ,@body
                                              )))))
              )))

(defmacro publish-ajax-func (path-or-options args &rest body)
  `(publish-ajax-update ,path-or-options
                        (let (,@(mapcar #'(lambda (arg)
                                            `(,arg (request-query-value ',(smart-string (string arg)) req)))
                                        args))
                          ,@body)))

(defvar *ajax-counter* 0)

(defmacro ajax-continuation ((&key args keep content-type no-session? name) &body body)
  `(let ((fname (string+ "/ajax/" ,(or name "g") "/" (fast-string (incf *ajax-counter*)))))
     (publish-ajax-func (:path fname :content-type ,content-type :no-session? ,no-session?) ,args
                        ,@body
                        ,(unless keep
                               '(unpublish fname)))
     fname))


;;; Inexplicably not in aserve.
(defun unpublish (path)
  (net.aserve::unpublish-entity (net.aserve::find-locator :exact *wserver*) path nil nil))

;; could keep this sorted I supposed
(defvar *responder-timeout* nil)

(defun set-responder-timeout (path &optional (time (+ (now) (* 5 60))))
  (push (list time path) *responder-timeout*))

(defun do-responder-timeouts ()
  (let* ((now (get-universal-time))
         (expired
          (mt:collecting
           (dolist (item *responder-timeout*)
             (when (> (car item) now)
               (unpublish (cadr item))
               (mt:collect item))))))
    (setf *responder-timeout* (nset-difference *responder-timeout* expired))))

(eval-when (:load-toplevel)
  (in-background "Responder timeout"
                 (loop
                    (sleep 60)
                    (do-responder-timeouts))))


#|
Here's a (stupid) example of use, assumes content is bound.

(html
 (dolist (word (slotv content #$crx:words))
   (html (:princ-safe word)
         (:princ "&nbsp;")))
 :newline :br
 (button-to-remote
  "Click to add a random word"
  (ajax-continuation
   (push-end (random-word) (slotv content #$crx:words))
   (render-replace-chunk (content-chunk content)))))
|#


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

;;; +++ move to some version of utils
(defun delete-keyword-arg (key arglist)
  (awhen (position key arglist)
         (if (zerop it)
             (setf arglist (cddr arglist))
             (setf (nthcdr it arglist) (nthcdr (+ it 2) arglist))))
  arglist)

(defun delete-keyword-args (keys arglist)
  (if (null keys) arglist
      (delete-keyword-arg (car keys) (delete-keyword-args (cdr keys) arglist))))

(defun link-to-remote (text url &rest remote-function-options &key html-options &allow-other-keys)
  (link-to-function text (apply #'remote-function url (delete-keyword-args '(:html-options) remote-function-options)) :html-options html-options))

(defun link-to-function (text js &key html-options)
  (html
   ((:a :href "#" :onclick js :do* html-options)
    (:princ text))))                    ;+++ has to be :princ rather than :princ-safe to allow image tags in text.  Should be rethought, maybe this should be a macro that wraps arbitrary html gen.

(defun button-to-remote (text url &rest options)
  (button-to-function text (apply #'remote-function url options)))

(defun button-to-function (text js &rest options)
  (html
   ((:input :type "button" :value text :onclick js))))

(defun checkbox-to-remote (text url checked? &rest options)
  (html
   ((:input :type :checkbox :if* checked? :checked "true" :onclick (apply #'remote-function url options))
    (:princ "&nbsp;")
    (:princ-safe text)
    )))

(defvar *uploader-html*
  (concatenate 'string
               "<div id='~a'></div>"
               "<script TYPE='text/javascript'>make_uploader('~a', '~a', '~a', ~a);</script>"
               ))

(defparameter *file-field-name* "Data")

(defun uploader (id url &optional isDrugrank)
  (format nil *uploader-html* id id url *file-field-name* (if isDrugrank "true" "false"))
  )

;;; Generate a remote function (javascript Ajax call)
;; ex: (remote-function "/new-chunk" :params `(:user ,user :type (:raw ,(format nil "$(~A).value" selector-id))))
;; returns something like:
;;  new Ajax.Request('/new-chunk', {"asynchronous":true,"parameters":{"user":"mt","type":$(selector23).value}}); return false;
#|
:form      If t, serialize the surrounding form, else use params
:params    List of (:key1 value1 ...), ignored if :form is t
:confirm   Ask user for confirmation first (value is the message)
:complete  Javascript to execute when action completes
:success   as :complete, but on success only
:failure   as :complete, but on failure only
:before    Javascript to run before the Ajax request
:after     Javascript to run after the Ajax request
:spinner   The ID of an elt, a spinner will be inserted after the elt before the Ajax request and removed when completed
:in-function?

|#

(defun remote-function (url &key form params (in-function? t) confirm before after spinner
                        success failure complete eval-scripts?)
  (when spinner
    (let ((spin-js (format nil "add_spinner('~A');" spinner))
          (nospin-js (format nil "remove_spinner('~A');" spinner)))
      (setf before (if before
                       (string+ before spin-js)
                       spin-js))
      (setf complete (if complete
                         (string+ nospin-js complete)
                         nospin-js))))
  (let ((result
         (format nil "new Ajax.Request('~A', ~A); ~:[~;return false;~]"
                 url
                 (json-options `(:asynchronous t
                                               :parameters ,(if form
                                                                '(:raw "Form.serialize(this)")
                                                                (json-options-transform params))
                                               ,@(if complete `("onComplete" (:raw ,(format nil "function(request){~A}" complete))))
                                               ,@(if success `("onSuccess" (:raw ,(format nil "function(request){~A}" success))))
                                               ,@(if failure `("onFailure" (:raw ,(format nil "function(request){~A}" failure))))
                                               ,@(if eval-scripts? `("evalScripts" t))
                                               ))
                 in-function?)))

    (when before (setf result (string+ before result)))
    (when after (setf result (string+ result after)))
    (when confirm (setf result (format nil "if (confirm('~A')) { ~A };" confirm result)))
    result))






