// functions to support the wuwei Lisp library

function add_spinner(after) {
    var s_id = after + '_spin';
    Element.insert($(after), {after: "<img src='/npublic/images/spinner.gif', id='" + s_id + "'>"});
}

function remove_spinner (after) {
    var s_id = after + '_spin';
    var elt = $(s_id);
    if (elt != null) {		// spinner may have been removed by an update
	Element.remove(elt);
    }
}

// autocomplete support

function setupAutocomplete(input, continuation) {
    var input = $(input);
    input.continuation = continuation;
    // on blur, if autocomplete hasn't happend, send the string to the continuation in case it can do something (+++ should be an option)
    // Event.observe($(input), 'blur', function (event) { 
    // 	if (input.continuation != null) {
    // 	    window.setTimeout(function () { // yes this is horrible, but this has to happen AFTER the click handler or you get the wrong thing
    // 		new Ajax.Request(continuation, {asynchronous:true, evalScripts:true, parameters: {value_string: $(input).value, id: $(input).id} });
    // 		input.continuation = null;
    // 	    },150);
    // 	}
    // });
}
		 

function postAutocomplete(text, li) {
    var continuation = text.continuation;
    var value = li.id;
    var value_string = li.innerHTML;
    var id = text.id
    if (continuation != null) {
	new Ajax.Request(continuation, {asynchronous:true, evalScripts:true, parameters: {value: value, value_string: value_string, id: id} });
//	text.continuation = null;
    }
}

// inplace editor support

// stupid that this isn't in the thing to being with.
Ajax.InPlaceEditorWithEmptyText = Class.create(Ajax.InPlaceEditor, {

  initialize : function($super, element, url, options) {
    if (!options.emptyText)        options.emptyText      = "click to edit…";
    if (!options.emptyClassName)   options.emptyClassName = "inplaceeditor-empty";
    $super(element, url, options);
    this.checkEmpty();
  },

  checkEmpty : function() {
    if (this.element.innerHTML.length == 0 && this.options.emptyText) {
      this.element.appendChild(
          new Element('span', { className : this.options.emptyClassName }).update(this.options.emptyText)
        );
    }
  },

  getText : function($super) {
      if (empty_span = this.element.select('.' + this.options.emptyClassName).first()) {
      empty_span.remove();
    }
    return $super();
  },

  onComplete : function($super, transport) {
    this.checkEmpty();
    return $super(transport);
  }
});
