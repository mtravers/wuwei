(in-package :asdf)

(defsystem :wuwei
    :name "Ajax and other tools for web development."
    :serial t
    :depends-on (:aserve :cl-json :mtlisp :3utils
			 :swframes)	;+++ temp!  Autocomplete refers to sw still
    :components 
    (
     (:file "package")
     (:file "aserve-patch")
     (:file "htmlgen-patch")
     (:file "config")
     (:file "bigstring")
     (:file "web")
     (:file "ajax-render")
     (:file "error")
     (:file "async")
     (:file "autocomplete")
     ))
