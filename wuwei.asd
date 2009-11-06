(in-package :asdf)

(defsystem :wuwei
    :name "Collaborative, Ajaxy version of the web-based listener"
    :serial t
    :depends-on (:aserve :cl-json :mtlisp :3utils);  :cl-smtp :cl-base64
    :components 
    (
     (:file "package")
     (:file "bigstring")
     (:file "web")
     (:file "htmlgen-patch")
     (:file "ajax-render")
     (:file "async")

#|
     (:file "session")
     (:file "config")
     (:file "history")
     (:file "login")
     (:file "nlistener")
     (:file "tree")
     (:file "doc")
     (:file "discussions")
     (:file "content")
     (:file "forms")
     (:file "frame-browse")
     (:file "publish")
     (:file "frame-grid")
     (:file "ext")
     (:file "annotate")
     (:file "graphviz")
     (:file "user-funs")
     (:file "heatmap")
|#
     ))
