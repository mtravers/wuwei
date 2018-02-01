(in-package :wu)

;; The :protocol :http/1.0 is to get around what looks like a bug in the client code that causes random
;; interpolated characters. We think this is because of chunking, and using the older http protocol forces that off.

(defun get-url (url &rest keys)
  (apply #'net.aserve.client::do-http-request url :protocol :http/1.0 keys))

;;; Alternate get-url that doesn't use net.aserve.client::do-http-request, which will sometimes get borked for no apparent reason.

(defun wget-url (url &key query)
  (uiop:with-temporary-file (:pathname temp-file)
    (let ((url (if query
                   (string+ url "?" (query-to-form-urlencoded query))
                   url)))
    (uiop:run-program `("wget" "-c" ,url "-O" ,(uiop:native-namestring temp-file)))
    (file-to-string temp-file))))

(defun get-url-with-backoff (url &rest keys)
  (multiple-value-bind (body response)
      (apply #'get-url url keys)
    (case response
      (200 body)
      (503 (print "Got 503, will retry...")
	   (sleep 1)
	   (apply #'get-url-with-backoff url keys))
      (t 
       (error "Bad http response ~A" response)))))
