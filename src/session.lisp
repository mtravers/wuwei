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

;;; Session management

(export '(cookie-value
	  with-session with-session-response def-session-variable delete-session new-session-hook
	  *aserve-request*))

;;: Variables and parameters 

;;; Bound by session handler to the session name (a keyword)
(defvar *session* nil)
;;; Dynamic bound to current request, makes life much easier
;;; Bound in with-session, but should be universal
;;; There is an aserve variable, but not exported so not a good idea to use: net.aserve::*worker-request*
(defvar *aserve-request* nil)
;;; +++ document...and hook up or delete
;;; (defparameter *default-login-handler* nil)


;;; Session store
#|
Theory:
- there are multiple session stores, each represented by a session store object
- each handles a certain set of variables,
- each is indexed by the same session id

- variables are CLOS objects that point to a symbol and have additional info about reading/writing

|#

;;;; :::::::::::::::::::::::::::::::: Utilities

;;; Signatures

(defun string-signature (string &optional (secret *session-secret*))
  #+ALLEGRO (let ((*print-base* 36)) (princ-to-string (excl:hmac-sha1-string secret string)))
  #-ALLEGRO (hmac-sha1-string string secret))

#-ALLEGRO
(defun hmac-sha1-string (string &optional (secret *session-secret*) (return :hex))
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret) :sha1)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array string))
    (ecase return
      (:hex (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac)))
      (:bytes (ironclad:hmac-digest hmac)))))

;;; Value is a list, gets written out as | separated values with the signature added
(defun signed-value (v &optional (secret *session-secret*))
  (let* ((base-string (format nil "~{~A|~}" v))
	 (sig (string-signature (string+ base-string secret))))
    (format nil "~A~A" base-string sig)))
  
;; Return value is a list of strings, or NIL if it doesn't verify
(defun verify-signed-value (rv &optional (secret *session-secret*))
  (when rv
    (ignore-errors			;if error, just don't verify
      (let* ((split-pos (1+ (position  #\| rv :from-end t)))
	     (content (subseq rv 0 split-pos))
	     (sig (subseq rv split-pos)))
	(when (equal sig
		     (string-signature (string+ content secret)))
	  (butlast #+ALLEGRO (excl:split-re "\\|" content)
		   #-ALLEGRO (mt:string-split content #\|)
		   ))))))

;;;; :::::::::::::::::::::::::::::::: Session Stores

(defvar *session-stores* nil)

(defclass session-store ()
  ((variables :initform nil :reader session-variables)
   ))

(defmethod initialize-instance :after ((store session-store) &rest ignore)
  (declare (ignore ignore))
  (push store *session-stores*))

;;; dev only
(defun reset-session-stores ()
  (setf *session-stores* nil))


;;; Assumes there will be at most one of each, seems safe
(defun find-or-make-session-store (class)
  (or (find class *session-stores* :key #'type-of)
      (make-instance class)))

(defun cookie-value (req name)
  (assocdr name (get-cookie-values req) :test #'equal))


;;;; :::::::::::::::::::::::::::::::: Session Variables

(defmacro def-session-variable (name &optional initform &key (store-type :memory) reader writer)
  `(progn
     (defvar ,name ,initform)
     (let ((var (make-instance 'session-variable
		  :symbol ',name
		  :reader ',reader
		  :writer ',writer
		  :initform ',initform)))
       (add-session-variable ,store-type var)
       )))

;;; +++ these ought to delete var from other stores, for development purposes
(defmethod add-session-variable ((type (eql :memory)) var)
  (add-session-variable (find-or-make-session-store 'in-memory-session-store) var))

(defmethod add-session-variable ((type (eql :cookie)) var)
  (add-session-variable (find-or-make-session-store 'cookie-session-store) var))

;;; These stores don't exist yet +++

(defmethod add-session-variable ((type (eql :file)) var)
  (add-session-variable (find-or-make-session-store 'file-session-store) var))

(defmethod add-session-variable ((type (eql :sql)) var)
  (add-session-variable (find-or-make-session-store 'sql-session-store) var))

;;; This is constant once all session vars are defined, so kind of wasteful(+++)
(defun all-session-variables ()
  (mapappend #'session-variables *session-stores*))

(defun all-session-variable-symbols ()
  (mapappend #'session-variable-symbols *session-stores*))

;;; +++ extend so nil arg returns default values
(defun all-session-variable-values (session)
  (mapappend #'(lambda (store) (session-values store session)) *session-stores*))

(defun save-session-variables (&optional (session *session*))
  (dolist (store *session-stores*)
    (session-save-session-variables store session)))

(defclass session-variable ()
  ((symbol :initarg :symbol :reader session-variable-symbol)
   (reader :initarg :reader :initform nil)
   (writer :initarg :writer :initform nil)
   (store :initarg :store)
   (initform :initarg :initform :initform nil :reader session-variable-initform)))

(defmethod print-object ((object session-variable) stream)
  (with-slots (symbol) object
    (print-unreadable-object (object stream :type t :identity t)
      (princ symbol stream))))

(defmethod session-variable-value ((ssv session-variable))
  (symbol-value (session-variable-symbol ssv)))

;;; temp theory -- all writing is in lisp syntax, reader/writer just transforms into readable if necessary

(defmethod write-session-variable-value ((ssv session-variable) stream)
  (with-slots (writer) ssv
    (let ((raw (session-variable-value ssv)))
      (if (and writer raw)
	  (write (funcall writer raw) :stream stream)
	  (write raw :stream stream))
      (write-char #\space stream))))

(defmethod read-session-variable-value ((ssv session-variable) stream)
  (with-slots (reader) ssv
    (let ((raw (read stream)))
      (if (and reader raw)
	  (funcall reader raw)
	  raw))))


;;;; :::::::::::::::::::::::::::::::: Response Generation



(defmethod session-variable-symbols ((store session-store))
  (with-slots (variables) store
    (mapcar #'session-variable-symbol variables)))


(defmethod add-session-variable ((store session-store) var)
  (with-slots (variables) store
    ;; +++ no, we want to update if its the same as existing
    (replacef var variables :key #'session-variable-symbol)))


;;;; :::::::::::::::::::::::::::::::: Memory Session Store

(defclass in-memory-session-store (session-store)
  ((sessions :initform (make-hash-table :test #'eq))))

(defmethod session-values ((store in-memory-session-store) session)
  (with-slots (sessions variables) store
    (or (gethash session sessions)
	(setf (gethash session sessions)
	      (mapcar #'(lambda (var) (eval (session-variable-initform var))) variables))
	)))

;; +++ rename these methods, they are on session-store not session
(defmethod session-save-session-variables ((store in-memory-session-store) session)
  (with-slots (sessions variables) store
    (setf (gethash session sessions)
	  (mapcar #'session-variable-value variables))))

(defmethod session-delete-session ((store in-memory-session-store) session)
  (with-slots (sessions variables) store
    (remhash session sessions)))

(defmethod reset-session-store ((store in-memory-session-store))
  (with-slots (sessions) store
    (clrhash sessions)))
  

;;;; :::::::::::::::::::::::::::::::: Cookie Session Store

;;; +++ warning overdesign, may throw some of this out in the interests of simplifying other things

;;; +++ needs a timer and sweeper...could just make last-use-time a session variable

;;; +++ I don't quite understand how cookie store can work, since cookie sets have to be done before
;;; generating the body of a response.  Possibly through some javascript, but then that will affect
;;; the page content (maybe breaking caching, argh).  Of course if we are buffering responses, like
;;; we do on most cwest methods, then it could work.
(defclass serialized-session-store (session-store)
  ((package :initform (find-package :wuwei)))) ;+++ temp

(defclass cookie-session-store (serialized-session-store) 
  ((secret)
   (cookie-name :initform (string+ *system-name* "-session"))
   ))

(defmethod initialize-instance :after ((store cookie-session-store) &rest ignore)
  (declare (ignore ignore))
  (recompute-secret store))

;;; Incorporate the variables into the secret.  That way, if they change, existing cookies
;;; will be invalidated, otherwise they will be mismatched.
(defmethod recompute-secret ((store cookie-session-store))
  (with-slots (variables secret) store
    (setf secret
	  (with-output-to-string (s)
	    (write-string *session-secret* s)
	    (dolist (v variables)
	      (write (session-variable-symbol v) :stream s))))))

(defmethod add-session-variable :after ((store cookie-session-store) var)
  (recompute-secret store))

;;; Encryption option would be good (see ironclad package)

(defmethod session-values ((store cookie-session-store) session)
  (unless *aserve-request*
    (error "attempt to get cookie session vars without binding *aserve-request*"))
  (with-slots (cookie-name variables package secret) store
    (let ((value (verify-signed-value (cookie-value *aserve-request* cookie-name) secret))
	  (*package* package))
      (if value
	  (with-input-from-string (s (cadr value))
	    (collecting
	      (dolist (var variables)
		(collect (report-and-ignore-errors
			      (read-session-variable-value var s))))))
	  (mapcar #'(lambda (var) (eval (session-variable-initform var))) variables)
	  ))))

(defmethod set-cookie-session-cookie ((store cookie-session-store) req)
  (with-slots (cookie-name) store
    (set-cookie-header req :name cookie-name :value (session-state-cookie-value store) :expires :never)))

(defmethod session-state-cookie-value ((store cookie-session-store))
  (with-slots (variables package secret) store
    (signed-value
     (list *session*
	   (with-output-to-string (s)
	     (let ((*print-readably* t)
		   (*print-pretty* nil)
		   (*package* package))
;	 (unless compact?
;	   (format s "~S " (mapcar #'session-variable-symbol variables)))
	 (dolist (var variables)
	   (write-session-variable-value var s)))))
     secret)))

;;; No-op (should make sure vars have not changed since header was written +++)
(defmethod session-save-session-variables ((store cookie-session-store) session)
  (set-cookie-session-cookie store *aserve-request*))

;;; +++ these need to get timed out, otherwise they will accumulate ad infinitum

(defmethod session-delete-session ((store cookie-session-store) session)
  (with-slots (cookie-name) store
    (set-cookie-header *aserve-request* :name cookie-name :value ""))) 

;;;; :::::::::::::::::::::::::::::::: Login, Session Creation, Etc

(defun gensym-session-id ()
  (keywordize (format nil "~A-~A" (machine-instance) (get-universal-time))))

(defmacro with-session-variables (&body body)
  `(let ((%val nil))
     (progv (all-session-variable-symbols) (all-session-variable-values *session*)
       (unwind-protect
	    (setq %val (progn ,@body))
	 (save-session-variables *session*)))
     %val))

;;; Note: has to be OUTSIDE with-http-response-and-body or equiv
;;; +++ login-handler is ignored?
;;; Assumes *session* set by with-session-vars, nil if invalid.
;;; no longer implemented, but maybe should be brought back  (login-handler '*default-login-handler*)
(defmacro with-session ((req ent &key) &body body)
  `(let* ((*aserve-request* ,req)
	 (*session* (get-session-id (find-or-make-session-store 'cookie-session-store) ,req)) ;+++ assume this validates
	 (new-session nil))
     (unless *session*
       (setq *session* (gensym-session-id)
	     new-session t))
     (with-session-variables
       (when new-session (new-session-hook ,req ,ent))
       (save-session-variables)		;save cookie variables, especially *session*
       ,@body
       (save-session-variables)		;we also save the session variables here; let's memory state vars work more easily
       )))

(defmethod get-session-id ((store cookie-session-store) req)
  (with-slots (cookie-name secret) store
    (let ((value (verify-signed-value (cookie-value *aserve-request* cookie-name) secret)))
      (when value
	(keywordize (first value))))))

;;; must be run inside with-session
(defmacro with-session-response ((req ent &key content-type no-save?) &body body)
  `(progn
     (assert *session* nil "With-session-response in bad context")
     (unless ,no-save? (save-session-variables *session*))
     (with-http-response (,req ,ent ,@(if content-type `(:content-type ,content-type)))
       (with-http-body (,req ,ent)
	 ,@body))))

(defun logout (req ent)
  (with-session (req ent)
    (delete-session)))

(defun delete-session (&optional (key *session*) store-class)
  (if store-class
      (session-delete-session (find-or-make-session-store store-class) key)
      (dolist (store *session-stores*) (session-delete-session store key))))



;;; applications can redefine this to do special actions to initialize a session
(defun new-session-hook (req ent)
  (declare (ignore req ent))
  )

;;; See session-debug page in eval-server.lisp



  
