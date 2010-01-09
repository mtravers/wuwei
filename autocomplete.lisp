(in-package :wu)

(export '(auto-complete-field 
	  auto-complete-field-sparql
	  in-place-field))

#|
Support for autocomplete and in-place-editor widgets

See http://wiki.github.com/madrobby/scriptaculous/ajax-autocompleter

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
    (:insert :bottom "body"		;+++ this is dependent on a specific layout...
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
					(funcall on-change value)
					;; you are supposed to send the value back as the body
					(write-string value *html-stream*))
		    (json:encode-json-to-string options)))))))


	 

