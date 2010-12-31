(in-package :asdf)

#+ALLEGRO
(require :aserve)

(when (find-package :json)
  (pushnew :cl-json *features*))

(defsystem :wuwei
    :name "Ajax and other tools for developing web applications"
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
     
     (:file "config")
     (:file "net-utils")
     (:file "web")
     (:file "ajax-render")
     (:file "session")
     (:file "debug-utils")
     (:file "error")
     (:file "async")
     (:file "autocomplete")
     (:file "dom-objects")
     ))

(defsystem :wuwei-examples
    :name "WuWei Examples"
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :license "MIT"
    :serial t
    :depends-on (:wuwei)
    :components 
    ((:module "examples"
	     :serial t
	     :components
	     ((:file "home")
	      (:file "render-update")
	      (:file "async")
	      (:file "state")
	      (:file "color")
	      (:file "autocomplete-freebase")
	      ))))


