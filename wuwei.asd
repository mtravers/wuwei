(in-package :asdf)

(defsystem :wuwei
    :name "Ajax and other tools for web development."
    :serial t
    :depends-on (:aserve :cl-json :mtlisp :3utils)
    :components 
    (
     (:file "package")
     (:file "config")
     (:file "bigstring")
     (:file "web")
     (:file "htmlgen-patch")
     (:file "ajax-render")
     (:file "error")
     (:file "async")
     ))
