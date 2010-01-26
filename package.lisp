
(defpackage :wuwei
  (:nicknames :wu)
  (:use :cl :mt :3utils
        :net.aserve :net.html.generator
	:lisp-unit)
  (:export #:uploader #:*file-field-name*)
  (:shadowing-import-from :mt "SET-EQUAL")
  )
