(in-package :wu)

;;; +=========================================================================+
;;; | Copyright (c) 2009, 2010  Mike Travers and CollabRx, Inc                |
;;; |                                                                         |
;;; | Released under the MIT Open Source License                              |
;;; |   http://www.opensource.org/licenses/mit-license.php                    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Author:  Mike Travers


(export '(auto-complete-field 
	  auto-complete-field-sparql
	  in-place-field))

#|
Support for autocomplete and in-place-editor widgets

See http://wiki.github.com/madrobby/scriptaculous/ajax-autocompleter

Requires a DOM element named "body" to control where the autocomplete box gets inserted.

 Todo: 
 - layout and bounds stuff.
 - idea: a greyed out type indicator by default (apparently not supported by scriptaculous -- but it ought to layer on top OK).
 - completion machinery for replacing box with frame on comp
     updateElement     
 - highlighting match (esp for :match-type :word)
 - style stuff should be pulled out

|#

(defun auto-complete-field (&key (id (string (gensym "id")))
			    name
			    value
			    options
			    completions-url
			    on-selected
			    (update (string+ id "_auto_complete"))
			    input-options
			    )
  #.(doc "Generate an HTML autocompletion field. Arguments below (all except completions-url are optional)"
"ID - the HTML ID of the element"
"NAME - the name of the field"
"VALUE - the current value of the field"
"OPTIONS - additional options to pass to the scriptaculous Ajax.Autocompleter object, in JSON form"
"COMPLETIONS-URL - a URL that supplies the completions"
"ON-SELECTED - a function that is called with the value, value string, and id of the selected option"
"UPDATE - the HTML ID of the autocompletion box")
  (flet ((default-option (optname value)
	   (unless (member optname options :key #'car :test #'equal)
	     (push (cons optname value) options))))
    (default-option "paramName" "prefix")
    (when on-selected
      (default-option "afterUpdateElement" 
	  `(:raw "postAutocomplete")))
    )
  (html
   ((:input :id id :name name :if* value :value value :do* input-options))
   (render-scripts
    ;; put the autocomplete div somewhere where it won't get clipped
    (:insert :bottom "body"		
	     (html ((:div :id update :class "auto_complete"))))
    ;; this complex tangle enables an action to be taken when a completion is selected.
    (:js (if on-selected (format nil "setupAutocomplete('~A', '~A');" id 
			      (ajax-continuation (:args (value value_string id) :name "ac_finish" :keep t) ;+++ :keep t should be unnecessary, but for some reason this gets called multiple times under some conditions
						 (funcall on-selected value value_string id)
						 ))))

    (:js (format nil "var ~A_auto_completer = new Ajax.Autocompleter('~A', '~A', '~A', ~A);"
		  id
		  id
		  update
		  completions-url
		  (json:encode-json-to-string options))))
   ))

;;; In-place editor (see http://wiki.github.com/madrobby/scriptaculous/ajax-inplaceeditor )
;;; :on-change function called with new value
(defun in-place-field (&key (id (string (gensym "id")))
		       name
		       options
		       (prompt "Click to edit.")
		       on-change
		       value
		       class
		       submit-on-blur?
		       )
  (when prompt
    (push `("emptyText" . ,prompt) options))
  (when submit-on-blur?
    (push `("submitOnBlur" . "true") options))
  (let ((current-value value))
    (html 
     ((:div :id id :name name :if* class :class class); :style "border:1px solid gray"
      (if current-value
	  (html (:princ-safe current-value))))
     (render-scripts
      (:js (format nil "new Ajax.InPlaceEditorWithEmptyText('~A', '~A', ~A);"
		    id
		    ;; :keep t permits multiple editings.
		    (ajax-continuation (:args (value) :content-type "text/text" :name "inplace" :keep t)
		      (when on-change (funcall on-change value))
		      ;; you are supposed to send the value back as the body
		      (write-string value *html-stream*))
		    (json:encode-json-to-string options)))))))


	 

