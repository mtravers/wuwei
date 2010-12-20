(in-package :asdf)

#+ALLEGRO
(require :aserve)

(when (find-package :json)
  (pushnew :cl-json *features*))

(defsystem :wuwei
    :name "Ajax and other tools for web development."
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :license "MIT"
    :serial t
    :depends-on (#-ALLEGRO :aserve #-:CL-JSON :cl-json :mtlisp)
    :components 
    (
     (:file "package")
     ;; Patches to existing systems
     #-ALLEGRO (:file "aserve-patch")
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
