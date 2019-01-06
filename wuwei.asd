(in-package :asdf)

#+ALLEGRO
(require :aserve)

(defsystem :wuwei
  :name "WuWei"
  :description "Tools for developing Ajaxy web applications"
  :long-description "WuWei is a toolkit for building Ajax web pages and web sites in Common Lisp. It's designed to be light-weight, a toolkit rather than a platform. Features include: Continuation-based AJAX user interfaces; Server-side DOM operations (add/remove elements, visual fades, drag and drop); High-level interfaces to in-place-editing and autocomplete widgets; Login and session management"
  :version "0.1"
  :author "Mike Travers <mt@hyperphor.com>"
  :license "MIT"
  :serial t
  :depends-on (#-ALLEGRO :aserve :cl-json :mtlisp #-ALLEGRO :ironclad 
			 :drakma)	;for oauth2
  :components 
  ((:static-file "wuwei.asd")
   (:module :src
	    :serial t      
	    :components
	    ((:file "package")

	     (:file "htmlgen-patch")
	     (:file "cl-json-patches")
     
	     (:file "config")
	     (:file "net-utils")
	     (:file "web")
	     (:file "ajax-render")
	     (:file "web2")
	     (:file "wu")
	     (:file "debug-utils")
	     (:file "session")
	     (:file "error")
	     (:file "upload")
	     (:file "async")
	     (:file "autocomplete")
	     (:file "dom-objects")
	     (:file "eval-server")
	     (:file "oauth2")
	     (:file "heroku")
	     ))))

(defsystem :wuwei-examples
    :name "WuWei Examples"
    :description "Example for WuWei"
    :version "0.1"
    :author "Mike Travers <mt@hyperphor.com>"
    :license "MIT"
    :serial t
    :depends-on (:wuwei :drakma)
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
	      (:file "go")		;set up for Heroku
	      ))))


