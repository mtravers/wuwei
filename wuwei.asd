(in-package :asdf)

#+ALLEGRO
(require :aserve)

(when (find-package :json)
  (pushnew :cl-json *features*))

(defsystem :wuwei
  :name "WuWei"
  :description "Tools for developing Ajaxy web applications"
  :long-description "WuWei is a toolkit for building Ajax web pages and web sites in Common Lisp. It's designed to be light-weight, a toolkit rather than a platform. Features include: Continuation-based AJAX user interfaces; Server-side DOM operations (add/remove elements, visual fades, drag and drop); High-level interfaces to in-place-editing and autocomplete widgets; Login and session management"
  :version "0.1"
  :author "Mike Travers <mt@hyperphor.com>"
  :license "MIT"
  :serial t
  :depends-on (#-ALLEGRO :aserve #-:CL-JSON :cl-json :mtlisp #-ALLEGRO :ironclad)
  :components 
  ((:static-file "wuwei.asd")
   (:module :src
	    :serial t      
	    :components
	    ((:file "package")

	     ;; Patches to existing systems
;;; I believe this is no longer necessary (oh but it is)
	     #-ALLEGRO (:file "aserve-patch")
	     (:file "htmlgen-patch")
	     (:file "cl-json-patches")
     
	     (:file "config")
	     (:file "net-utils")
	     (:file "web")
	     (:file "ajax-render")
	     (:file "wu")
	     (:file "session")
	     (:file "debug-utils")
	     (:file "error")
	     (:file "upload")
	     (:file "async")
	     (:file "autocomplete")
	     (:file "dom-objects")
	     (:file "eval-server")
	     ))))

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
	      (:file "arc-challenge")
	      ))))


