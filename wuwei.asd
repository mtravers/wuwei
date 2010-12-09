(in-package :asdf)

(defsystem :wuwei
    :name "Ajax and other tools for web development."
    :serial t
    :depends-on (:aserve :cl-json :mtlisp 
;;; can we survive? +++			 :3utils
			 )
    :components 
    (
     (:file "package")
     ;; Patches to existing systems
     (:file "aserve-patch")
     (:file "htmlgen-patch")
     (:file "cl-json-patches")
     ;; Config
     (:file "config")
     (:file "net-utils")
     (:file "bigstring")
     (:file "web")
     (:file "ajax-render")
     (:file "session")
     (:file "debug-utils")
     (:file "error")
     (:file "async")
     (:file "autocomplete")
     (:file "dom-objects")
     ))
