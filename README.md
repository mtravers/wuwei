# WuWei -- an Ajaxy web toolkit for Common Lisp

WuWei is a toolkit for building Ajax web pages and web sites in Common Lisp. It's designed to be
light-weight, a toolkit rather than a platform.  

[Wu wei is Chinese](http://en.wikipedia.org/wiki/Wu_wei) for "effortless doing".  

## Features
* Continuation-based AJAX user interfaces
* Server-side DOM operations (add/remove elements, visual fades, drag and drop)
* High-level interfaces to in-place-editing and autocomplete widgets
* Login and session management
* OAuth2 client

## Examples

May be run at the demo site: [http://wuwei.name](http://wuwei.name)

## Credits

  WuWei was written primarily by Mike Travers, originally under the sponsorship of CollabRx, Inc.
  Some bits of it are derived from [BioBike](http://biobike.org).

## Requirements/dependencies:

* A Common Lisp implementation.  WuWei has been run in Allegro, Clozure (aka OpenMCL), and SBCL, and ought to run in other implementations.
* [mtlisp](https://github.com/mtravers/mtlisp), a utility package.
* Other libraries (aserve or portable aserve, cl-json, and their dependencies)
* Uses the Prototype and Scriptaculous JavaScript libraries 
  (included in the source).

## Install

Easiest using QuickLisp:

1. Install QuickLisp from http://www.quicklisp.org/
2. Tell the install system where to find wuwei and mtlisp:

     (push #p"/misc/repos/wuwei/" asdf:*central-registry*)
     (push #p"/misc/repos/mtlisp/")

3. (OpenMCL only) Unfortunately there are some incompatabilities with the distributed version of Portable AllegroServe and OpenMCL, so:
  - download [my patched version of aserve](https://github.com/mtravers/aserve)
  - From that directory:

    (load "acl-compat/acl-compat.asd")
    (load "aserve/aserve.asd")

4. Load everything:

     (ql:quickload "wuwei")
     (ql:quickload "wuwei-examples")  ; if wanted

5. Start the server

    (net.aserve:start :port 3333)

