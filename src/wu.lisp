(in-package :wu)

;;; +=========================================================================+
;;; | Copyright (c) 2009-2011  Mike Travers and CollabRx, Inc                |
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

#|
Provide slightly higher-level functions for the common case, to make
cleaner looking web code.
|#

;;; Publish an explict URL, body is wrapped in lamda, response generation, HTML
;;; +++ Could extend URL to be list of url + options (eg content-type)
(defmacro wu-publish (url &body body)
  `(publish :path ,url :content-type "text/html"
		       :function #'(lambda (req ent) 
				   (with-http-response-and-body (req ent)
				     (html
				      ,@body)))))

;;; As above, A continuation that returns HTML (ajax-continuation by default returns javascript)
;;; OPTIONS are passed to AJAX-CONTINUATION
;;; Body is wrapped in HTML macro
(defmacro wu-continuation (options &body body)
  `(ajax-continuation (:content-type "text/html" ,@options )
      (html
	,@body)))

;;; Generate a sequential series of actions, each a continuation of the next (eg a web progn).
;;; Each clause is of the form (<options> . <body>), options are passed to wu-continuation.
;;; Clauses refer to the next page via (-next-).
;;; For an example, see examples/arc-challenge.lisp
;;; Note: this is something you probably never really want to do in a real web app. I wrote this in response
;;; to the ARC challenge.  If you DID, you'd probably want to enable the back button by putting :keep t in
;;; the options.

(defmacro wu-conversation (first &body rest)
  `(macrolet ((-next- () `(wu-conversation-1 ,@',rest))) ;don't feel sad, it only took me the better part of a day to get this quoting right!
     (html ,@(cdr first))))

(defmacro wu-conversation-1 (first &body rest)
  (when first
    `(macrolet ((-next- () `(wu-conversation-1 ,@',rest)))
       (wu-continuation ,(car first)
	 (html ,@(cdr first))))))





