(in-package :asdf)

(defsystem :wuwei
    :name "Ajax and other tools for web development."
    :serial t
    :depends-on (:aserve :cl-json :mtlisp :3utils)
    :components 
    (
     (:file "package")
     (:file "aserve-patch")
     (:file "htmlgen-patch")
     (:file "cl-json-patches")
     (:file "config")
     (:file "bigstring")
     (:file "web")
     (:file "ajax-render")
     (:file "debug-utils")
     (:file "error")
     (:file "async")
     (:file "autocomplete")
     ))
