(in-package :wu)

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
		 
