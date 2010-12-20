(defpackage :wuwei
  (:nicknames :wu)
  (:use :cl :mt
        :net.aserve :net.html.generator
	:clos*				;added for dom-objects, not necessary otherwise
	:lisp-unit)
;;; exports are within the source files
;  (:export #:uploader #:*file-field-name*)
  (:shadowing-import-from :mt "SET-EQUAL")
  )
