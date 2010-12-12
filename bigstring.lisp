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


;;; Supports embedding big strings into Lisp code, using syntax:
;;; #[[ this
;;; is a big and multiline
;;; string. Deal with it.]]

;;; Works OK, but screws up Emacs of course.

;;; Also, end sequence ought to be something less likely to be used.

(set-dispatch-macro-character #\# #\[ 'bigstring-reader)

(defun bigstring-reader (stream char arg)
  (declare (ignore char arg))
  (unless (char= #\[ (read-char stream))
    (error "Illegal #[ syntax"))
  (do ((result "")
       (line (read-line stream) (read-line stream))
       fin)
    ((setq fin (search "]]" line)) 
     (concatenate 'string result (subseq line 0 fin)))
    (setf result (concatenate 'string result line (string #\Newline)))))
		 
