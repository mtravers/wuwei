(in-package :wu)

(export '(auto-complete-field 
	  auto-complete-field-sparql
	  in-place-field))

#|
Support for autocomplete and in-place-editor widgets

See http://wiki.github.com/madrobby/scriptaculous/ajax-autocompleter

 Todo: 
 - should decouple Frame and SPARQL from a more basic layer
   -  started.  Decouple from frames as well?
 - layout and bounds stuff.
 - idea: a greyed out type indicator by default (apparently not supported by scriptaculous -- but it ought to layer on top OK).
 - completion machinery for replacing box with frame on comp
     updateElement     
 - highlighting match (esp for :match-type :word)
 - style stuff should be pulled out
|#

#| Tests

(raw-html
 (nl::html-string
  (html ((:div :style "height:200px;font-family:Gill Sans")
	 (nl::auto-complete-field-sparql :sparql '(:from #$http://collabrx.com/omim
					    :clauses ((?s #$rdf:type #$http://bio2rdf.org/ns/omim#GeneticDisorder))
					    :match-type :word))


(raw-html
 (nl::html-string
  (nl::auto-complete-field-sparql :sparql '(:from #$http://purl.org/science/graph/ncbi/gene-info
				     ;; ARGH, apparently this triple is in a separate graph!  
				     :clauses ();(?s #$rdf:type #$http://purl.org/science/owl/sciencecommons/gene_record)
				     :predicate #$http://purl.org/science/owl/sciencecommons/ggp_has_symbol))
  (nl::html ((:div :style "height:200px")))))

|#

(defun auto-complete-field (&key (id (string (gensym "id")))
			    name
			    options
			    url
			    (update (string+ id "_auto_complete"))
			    frame
			    predicate
			    )
  (flet ((default-option (optname value)
	   (unless (member optname options :key #'car :test #'equal)
	     (push (cons optname value) options))))
    (default-option "paramName" "prefix")
    (default-option "afterUpdateElement" '(:raw "postAutocomplete")) ;may need to be :raw
    )
  (html
   ((:input :id id :name name))
   (render-scripts
    ;; put the autocomplete div somewhere where it won't get clipped
    (:insert :bottom "main"		;+++ this is dependent on a specific layout...
	     (html ((:div :id update :class "auto_complete"))))
    (:js (formatn "var ~A_auto_completer = new Ajax.Autocompleter('~A', '~A', '~A', ~A);"
		  id
		  id
		  update
		  url
		  (json:encode-json-to-string options))))
   (when frame
     (render-scripts
      (:js (formatn "setupAutocomplete('~A', '~A', '~A');" id (frame-name frame) (frame-name predicate)))))

   ))

(defun delete-keyword-arg (key arglist)
  (awhen (position key arglist)
	 (if (zerop it)
	     (setf arglist (cddr arglist))
	     (setf (nthcdr it arglist) (nthcdr (+ it 2) arglist))))
  arglist)

(defun auto-complete-field-sparql (&rest rest &key sparql &allow-other-keys )
  (apply #'auto-complete-field 
   :url 
   (ajax-continuation+ (:args (prefix) :keep t) 
		       (html
			(:ul
			 (dolist (binding
				   (apply #'autocomplete-sparql prefix :uris t sparql ))
			   (let ((term (sw::sparql-binding-elt binding '?term))
				 (frame (sw::sparql-binding-elt binding '?s )))
			     (html
			      ((:li :id (frame-name frame))
			       (:princ-safe term))))))))
   (delete-keyword-arg :sparql rest)))

(defun autocomplete-sparql (prefix &key clauses (limit 10) from (predicate #$rdfs:label) uris (match-type :prefix))
  (funcall (if uris
	       #'do-sparql
	       #'do-sparql-one-var)
	   *default-frame-source*
	   `(:select (?term ?s) (:limit ,limit :from ,from :distinct t :order ?term)
		     (?s ,predicate ?term)
		     ,@clauses
		     (:filter (:regex ?term ,(formatn (case match-type
							(:prefix "^~A")
							(:word "\\\\b~A") 
							(:any "~A"))
						      prefix) "i")))))

(publish-ajax-func "/autocomplete-finish" (id frame predicate value value_string)
		   (let ((frame (intern-uri frame))
			 (predicate (intern-uri predicate))
			 (value (intern-uri value)))
		     (write-slot frame predicate :value value) 
		     (render-update 
		      (:remove (string+ id "_auto_complete")) ;+++ will break if there is an :update argument.
		      (:replace id 
				(emit-frame-link value :pre-text value_string)))))


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
      (:js (formatn "new Ajax.InPlaceEditorWithEmptyText('~A', '~A', ~A);"
		    id
		    (ajax-continuation+ (:args (value) :content-type "text/text")
					(write-slot frame predicate :value value) 
					;; you are supposed to send the value back as the body
					(write-string value *html-stream*))
		    (json:encode-json-to-string options)))))))
