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
Copied/moved from BioLisp

A simple Ajaxy way to compute results asynchronously  

  (async (prove-fermats-last-theorem))

in a listener will return immediately a value that gets rendered as a DIV with a spinner,
together with an Ajax call to the server, which computes the value in the background
and returns it when ready.

Note: at the moment, getting this to work requires a patch in aserve/main.cl:
because the form that comes back actually says something like:
		    "application/x-www-form-urlencoded; charset utf-8"

See header-first-field

Todo:
- have a settable pre-fetch string
- ht should be weak; does Lisp even support that?
   CCL does, see ccl::make-weak-table
|#


(defstruct async-result 
  id
  thunk
  (finished? nil)
  result
  pre-text)

(defvar *async-result-ht* (make-hash-table :test #'equal))

(defun make-async-result-1 (&rest keys)
  (let ((res (apply #'make-async-result keys)))
    (setf (gethash (async-result-id res) *async-result-ht*) res)
    res))

;;; Maybe track if .js has been issued based on net.aserve:*worker-request*?
(defmethod out-record-to-html ((form async-result) (string string) &rest ignore)
  (declare (ignore ignore))
  (if (async-result-finished? form)
      (html
	(:princ (async-result-result form)))
      (html 
       ;; for debugging add:  :style "border-width:1px;border-style:solid;"
	((:span :id (async-result-id form) )
	 (aif (async-result-pre-text form)
	      (html (:princ-safe it)))
	 ((:img :src (image-url "spinner.gif"))))
	(:newline)
	((:script :type "text/javascript")
	 (:newline)
	 (:princ (format nil "Async.getAsyncResult('~A');" (async-result-id form)))
	 (:newline)
	 ))
      ))
  
(defmacro async ((&key (pre-text "Wait...")) &body body)
  `(make-async-result-1
    :pre-text ,pre-text
    :id  (string (gensym "lz"))
    :thunk #'(lambda () ,@body)))

(defmacro async-html ((&key pre-text) &body body)
  `(out-record-to-html
    (async (:pre-text ,pre-text)
	   (with-output-to-string (out)
	     (let ((*html-stream* out))
	       ,@body)))
    "useless"))

(export '(async))

(publish :path "/async/get"
	 :function 'handle-async-request)

(defun handle-async-request (req ent)
  (let* ((id (net.aserve::request-query-value "id" req))
	 (async (gethash id *async-result-ht*))
	 thunk result)
    (if async
	(progn
	  (setf thunk (async-result-thunk async)
		result (funcall thunk))
	  (setf (async-result-result async) result
		(async-result-finished? async) t)
	  (with-http-response (req ent)
	    (with-http-body (req ent)
	      (write-string result net.aserve::*html-stream*))))
	;; should be a net error of some kind
	(error "Couldn't find async request ~A" id)
	)))

