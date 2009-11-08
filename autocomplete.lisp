(in-package :wu)

(export '(auto-complete-field 
	  auto-complete-field-sparql
	  in-place-field))

#|
Support for autocomplete and in-place-editor widgets

See http://wiki.github.com/madrobby/scriptaculous/ajax-autocompleter

 Todo: 
 - should decouple Frame and SPARQL from a more basic layer
   -  Autocomplete is done, in-place todo
 - layout and bounds stuff.
 - idea: a greyed out type indicator by default (apparently not supported by scriptaculous -- but it ought to layer on top OK).
 - completion machinery for replacing box with frame on comp
     updateElement     
 - highlighting match (esp for :match-type :word)
 - style stuff should be pulled out
|#


(defun auto-complete-field (&key (id (string (gensym "id")))
			    name
			    options
			    completions-url
			    on-selected
			    (update (string+ id "_auto_complete"))
			    )
  (flet ((default-option (optname value)
	   (unless (member optname options :key #'car :test #'equal)
	     (push (cons optname value) options))))
    (default-option "paramName" "prefix")
    (when on-selected
      (default-option "afterUpdateElement" 
	  `(:raw "postAutocomplete")))
    )
  (html
   ((:input :id id :name name))
   (render-scripts
    ;; put the autocomplete div somewhere where it won't get clipped
    (:insert :bottom "main"		;+++ this is dependent on a specific layout...
	     (html ((:div :id update :class "auto_complete"))))
    ;; this complex tangle enables an action to be taken when a completion is selected.
    (:js (if on-selected (format nil "setupAutocomplete('~A', '~A');" id 
			      (ajax-continuation (:args (value value_string id))
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

(defun in-place-field (&key (id (string (gensym "id")))
		       name
		       options
		       frame
		       predicate
		       (prompt "Click to edit.")
		       )
  (when prompt
    (push `("emptyText" . ,prompt) options))
  (let ((current-value (and frame (ssv frame predicate))))
    (html 
     ((:div :id id :name name); :style "border:1px solid gray"
      (if current-value
	  (html (:princ-safe current-value))))
     (render-scripts
      (:js (format nil "new Ajax.InPlaceEditorWithEmptyText('~A', '~A', ~A);"
		    id
		    (ajax-continuation+ (:args (value) :content-type "text/text")
					(write-slot frame predicate :value value) 
					;; you are supposed to send the value back as the body
					(write-string value *html-stream*))
		    (json:encode-json-to-string options)))))))
