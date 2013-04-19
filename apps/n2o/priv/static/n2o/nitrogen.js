// vim: sw=4 ts=4 et
// The idea is to have one high level
// Nitrogen object, created from NitrogenClass, that 
// encapsulates everything in order to prevent collisions.

function NitrogenClass(o) {
    this.$url = document.location.href;
    this.$div = document;
    this.$anchor_root_path = document;
    this.$params = new Object();
    this.$event_queue = new Array();
    this.$event_is_running = false;
    this.$system_event_queue = new Array();
    this.$system_event_is_running = false;
    this.$system_event_obj = null;
    this.$going_away = false;
    this.$live_validation_data_field = "LV_live_validation";
    return this;
}

/*** PRIVATE METHODS ***/

NitrogenClass.prototype.$path_alias = function(path) {
    if (path === 'page') {
        return document;
    } else {
        return path;
    }
}

NitrogenClass.prototype.$anchor = function(anchor, target) {
    this.$anchor_path = this.$path_alias(anchor);
    this.$target_path = this.$path_alias(target);
}


NitrogenClass.prototype.$anchor_root = function(anchor_root) {
    this.$anchor_root_path = anchor_root;
}

NitrogenClass.prototype.$set_param = function(key, value) {
    this.$params[key] = value;
}

NitrogenClass.prototype.$destroy = function() {
    document.comet_started = false;
    this.$going_away = true;

    // Clear the system event queue and abort any pending system events.
    this.$system_event_queue = new Array();
    if( this.$system_event_obj !== null ) {
    this.$system_event_obj.abort();
    }
    this.$system_event_is_running = false;

    // Let the event loop keep running until the event queue is empty. 
}


/*** EVENT QUEUE ***/

NitrogenClass.prototype.$queue_event = function(validationGroup, eventContext, extraParam, ajaxSettings) {
    // Put an event on the event_queue.
    this.$event_queue.push({
        validationGroup : validationGroup,
        eventContext    : eventContext,
        extraParam      : extraParam,
        ajaxSettings    : ajaxSettings
    });
}

NitrogenClass.prototype.$queue_system_event = function(eventContext) {
    // Put an event on the event_queue.
    this.$system_event_queue.push({
        eventContext : eventContext
    });
}

NitrogenClass.prototype.$event_loop = function() {
    // Create a local copy of this for setTimeout callbacks.
    var this2 = this;

    // If no events are running and an event is queued, then fire it.
    if (!this.$system_event_is_running && this.$system_event_queue.length > 0) {
        var o = this.$system_event_queue.shift();
        this.$do_system_event(o.eventContext);
    }

    // If no events are running and an event is queued, then fire it.
    if (!this.$event_is_running && this.$event_queue.length > 0) {
        var o = this.$event_queue.shift();
        this.$do_event(o.validationGroup, o.eventContext, o.extraParam, o.ajaxSettings);
    }

    if (this.$system_event_queue.length == 0 || this.$event_queue.length == 0) {
    if( this.$going_away ) {
        // $destroy has been called for this Nitrogen object
        // and the event queue is empty - let the event loop stop.
        return;
    }
    else {
        // No more events, sleep for 50 ms...
        setTimeout( function() { this2.$event_loop() }, 50);
        return;
    }
    }

    // Events queued, but one is running, sleep for 10 ms...
    if (this.$event_is_running || this.$system_event_is_running) {
        setTimeout( function() { this2.$event_loop() }, 10);
        return;
    }

    // Events queued, loop and grab it...
    setTimeout( function() { this2.$event_loop() }, 1);
}

/*** VALIDATE AND SERIALIZE ***/

NitrogenClass.prototype.$validate_and_serialize = function(validationGroup) {
    // Check validatation, build object of params...
    var is_valid = true,
        params= {},
        n = this;

    jQuery(":input").not(".no_postback").each(function(i) {
        var LV = Nitrogen.$get_validation(this);
        if (LV && LV.group == validationGroup && !LV.validate()) {
            // Set a flag, but keep validating to show all messages.
            is_valid = false;
        } else {
            // Skip any unchecked radio boxes.
            if ((this.type == "radio" || this.type=="checkbox") && !this.checked) return;
            params[n.$make_id(this)] = this.value;
        }
    });
    // Return the params if valid. Otherwise, return null.
    return is_valid && params || null;
}

NitrogenClass.prototype.$add_validation = function(element, args) {
    if($(element)){
        if(!$(element).data(Nitrogen.$live_validation_data_field))
            $(element).data(Nitrogen.$live_validation_data_field, new LiveValidation(element, args));
        return Nitrogen.$get_validation(element);
    } else
        return null;
}

NitrogenClass.prototype.$get_validation = function(element) {
    return $(element).data(Nitrogen.$live_validation_data_field);
}

// TODO: This needs to be made smarter. Right now, I'm pretty sure elements have
// single validation groups, while it should be a list of groups that get validated
NitrogenClass.prototype.$destroy_specific_validation = function(trigger, target) {
    var v = NitrogenClass.$get_validation(target);
    if(v.group==trigger)
        Nitrogen.$destroy_target_validation(element);
}

NitrogenClass.prototype.$destroy_target_validation = function(element) {
    var v = Nitrogen.$get_validation(element);
    if(v) {
        v.destroy();
        $(element).data(Nitrogen.$live_validation_data_field,null);
    }
}

NitrogenClass.prototype.$destroy_validation_group = function(validationGroup) {
    jQuery(":input").not(".no_postback").each(function(i) {
        var LV = Nitrogen.$get_validation(this);
        if( LV && LV.group == validationGroup) {
            Nitrogen.$destroy_target_validation(this);
        }
    });
}

NitrogenClass.prototype.$destroy_all_validation = function() {
    $("*").each(function() {
        Nitrogen.$destroy_target_validation(this);
    });
}

NitrogenClass.prototype.$make_id = function(element) {
    var a = [];
    var re = new RegExp("\.wfid_(.[^\\s]*)", "g");
    while(element && element.className) {
        var matches = element.className.match(/wfid_([^\s])+/g);
        if (matches) {
            a.unshift.apply(a, matches);
        }       
        element = element.parentNode;
    }  
    return a.join(".");
}


/*** AJAX METHODS ***/

NitrogenClass.prototype.$do_event = function(validationGroup, eventContext, extraParam, ajaxSettings) {
    var n = this;
    
    var s = jQuery.extend({
        dataType: 'text',
        cache: false,
        success: null,
        error: null
    }, ajaxSettings);
    
    // Flag to prevent firing multiple postbacks at the same time...
    this.$event_is_running = true;

    // Run validation...
    var validationParams = this.$validate_and_serialize(validationGroup);   
    if (validationParams == null) {
        this.$event_is_running = false;
        return;
    }

    // Assemble other parameters...
    var params = jQuery.extend({}, n.$params, validationParams, { eventContext: eventContext });
    
    jQuery.ajax({ 
        url: this.$url,
        type:'post',
        data: [jQuery.param(params), extraParam || ''].join('&'),
        dataType: s.dataType,
        cache: s.cache,
        success: function(data, textStatus) {
          n.$event_is_running = false;
          typeof s.success  == 'function' && s.success(data, textStatus) || eval(data);
            },
            error: function(xmlHttpRequest, textStatus, errorThrown) {
          n.$event_is_running = false;
          typeof s.success == 'function' && s.error(xmlHttpRequest, textStatus, errorThrown);
        }
    });         
}

/*** SYSTEM EVENTS (FOR ASYNC) ***/

NitrogenClass.prototype.$do_system_event = function(eventContext) {
    var n = this;
    // Flag to prevent firing multiple postbacks at the same time...
    n.$system_event_is_running = true;

    // Assemble other parameters...
    var params = jQuery.extend( {}, n.$params, { eventContext: eventContext, is_system_event: 1 });

    n.$system_event_obj = $.ajax({
           url: this.$url,
           type:'post',
           data: jQuery.param(params),
           dataType: 'text',
         cache: false,
           success: function(data, textStatus) {
            n.$system_event_is_running = false;
            n.$system_event_obj = null;
            // A system event shouldn't clobber the pageContext.
            // Easiest to cacount for it here.
            var pc = n.$params["pageContext"];
            eval(data);
            n.$set_param("pageContext", pc);
           },
           error: function(xmlHttpRequest, textStatus, errorThrown) {
            n.$system_event_is_running = false;
            n.$system_event_obj = null;
           }
       });                     
}

/*** FILE UPLOAD ***/
/*NitrogenClass.prototype.$upload = function(form,input) {
    // Assemble other parameters...
    form.action = this.$url;
    form.pageContext.value = this.$params["pageContext"];
    form.submit();
    form.reset();
}*/

/*** GMAIL-STYLE UPLOAD ***/

NitrogenClass.prototype.$send_pending_files = function(form,input) {
    var file=null;
    if(typeof(form.$nitrogen_pending_files)=="object")
    {
        // not a typo, doing an assignment here
        while(file=form.$nitrogen_pending_files.shift())
        {
            file.submit();
        }
    }
}

NitrogenClass.prototype.$attach_upload_handle_dragdrop = function(form,input,settings) {
    var thisNitro = this;
    if(typeof(settings)=="undefined")
        settings={};
    if(typeof(form.$nitrogen_pending_files)=="undefined")
        form.$nitrogen_pending_files = [];

    jQuery.getScript("/nitrogen/jquery.fileupload.min.js",function(){
        var dropzone = jQuery(form).children(".upload_drop");
    
        jQuery(input).fileupload({
            dropZone:(settings.droppable ? dropzone : null),
            singleFileUploads:true,
            sequentialUploads:true,
            url:thisNitro.$url,
            paramName:"file",
            formData: function() {
                form.elements["pageContext"].value = thisNitro.$params["pageContext"];
                var d = jQuery(form).serializeArray();
                return d;
            },
            start: function(e) {
                form.pageContext.value = thisNitro.$params["pageContext"];
                jQuery(form).children(".upload_progress").fadeIn().text("Uploading...");
            },
            progressall: function(e,data) {
                var prog = parseInt(data.loaded / data.total * 100,10);
                // TODO: Convert this to a progress bar
                // Neede to add #progress{} element to continue with that
                jQuery(form).children(".upload_progress").text(prog + "% (" + data.loaded + "/" + data.total + " bytes)");
            },
            progress: function(e,data) {
                // Single file progress
            },
            send: function(e,data) {
            },
            stop: function(e,data) {
                
            },
            always: function(e,data) {
            },
            fail: function(e,data, options) {
                Nitrogen.$increment_pending_upload_counter(form,-1);
            },
            add: function(e,data) {
                jQuery.each(data.files,function(i,f) {
                    // Let's add the visual list of pending files
                    jQuery(form).children(".upload_droplist")
                        .prepend(jQuery("<li></li>").attr("filename",f.name).text(f.name));
                    Nitrogen.$increment_pending_upload_counter(form,1);
                });
                if(settings.autoupload)
                    data.submit();
                else
                    form.$nitrogen_pending_files.push(data);
            },
            done: function(e,data) {
                if(typeof data.result == "string") {
                    // Good browsers will use XHR file transfers, and so this
                    // will return a string
                    var Postback = data.result;
                } else if(typeof data.result == "object") {
                    // Crappy browsers (IE9 and below) will do the transfer
                    // as with an iframe and return a document-type object
                    var Postback = data.result[0].body.innerHTML;
                } else {
                    // IE also has data.result as "undefined" on failure
                    // So let's just treat it as an empty string
                    var Postback = "";
                }

                jQuery.globalEval(Postback);
                Nitrogen.$increment_pending_upload_counter(form,-1);
            }
        })
    })
}

NitrogenClass.prototype.$increment_pending_upload_counter = function(form,incrementer) {
    var counter = $(form).data("pending_uploads");
    if(typeof(counter)=="undefined")
        counter=0;
    counter+=incrementer;
    $(form).data("pending_uploads",counter);
    if(counter==0)
    {
        jQuery(form).children(".upload_progress").fadeOut();
        Nitrogen.$alert_unfinished_files(form);
    }
}


NitrogenClass.prototype.$upload_finished = function(Name) {
    jQuery(".upload_droplist").children("li[filename=\"" + Name + "\"]")
        .css("text-decoration","line-through")
        .addClass("upload_successful")
        .fadeOut();
}

NitrogenClass.prototype.$alert_unfinished_files = function(form) {
    var files = $(form).find(".upload_droplist li:not(.upload_successful):visible");
    if(files.length > 0)
    {
        $(form).find(".upload_droplist li:not(.upload_successful)").css("color","red").fadeOut("slow");

        var filenames = $(files).get().map(function(f) { return $(f).text() }).join("\r\n");
        alert("There was an error uploading the following file(s):\r\n" + filenames + "\r\n\r\nThis is likely due to the file(s) being too large or a misconfiguration on the server");
    }
} 


/*** PATH LOOKUPS ***/

function obj(path, anchor) {
    return objs(path, anchor).get(0);
}

function objs(path, anchor) {
    // Trim the path...
    path = jQuery.trim(path);

    // If no anchor is specified, then use the last anchor set...
    if (!anchor) {
        anchor = Nitrogen.$anchor_path;
    } else {
        anchor = Nitrogen.$path_alias(anchor);
    }

    // Multiple parts, so split and combine results...
    if (path.indexOf(",") != -1) {
        var paths=path.split(",");
        var a = $();
        for (var i=0; i<paths.length; i++) {
            a = a.add(objs(paths[i], anchor));
        }
        return a;
    }

    // Selector is "page", so return the document...
    if (path == "page" || path == ".page") {
    return jQuery(document);
    }

    // Replace "##" with ".wfid_"...
    path = path.replace(/##/g, '.wfid_');

    // Replace "me" with anchor...
    path = path.replace(/\bme\b/g, anchor);

    // If this is a single word, then rewrite it to a Nitrogen element id.
    if (path.indexOf(" ") == -1 && path.indexOf(".") == -1 && path.indexOf("#") == -1) {
        var results = objs(".wfid_" + path, anchor);
        
        // If we found results, then return them...
        if (results.length > 0) {
            return results;
        }

        // If no results, and this is not a valid HTML element name, then return. Otherwise,
        // keep trying with the assumption that this is an HTML element...
        if (results.length == 0 && jQuery.inArray(path.toLowerCase(), Nitrogen.$valid_elements) == -1) {
            return jQuery();
        }
    }

    // If path begins with "body", then try matching across the entire
    // body...
    var re = new RegExp(/^body\b/);
    if (re.test(path)) {
        return jQuery(path);
    }    

    var anchor_obj = jQuery(Nitrogen.$anchor_root_path).find(anchor);
    // Find all results under the anchor...
    var results = anchor_obj.find(path);
    if (results.length > 0) {
    return results;
    }
    
    // If no results under the anchor, then try on each parent, moving upwards...
    var results = anchor_obj.parentsUntil( Nitrogen.$anchor_root_path );
    for (var i=0; i<results.length; i++) {
    var results2 = jQuery(results.get(i)).find(path);
    if (results2.length > 0) {
        return results2;
    }       
    }

    // No results, so try in context of entire page.
    return jQuery(path);
}

NitrogenClass.prototype.$valid_elements = [
    "a", "abbr", "acronym", "address", "applet", "area", "b", "base", "basefont", 
    "bdo", "big", "blockquote", "body", "br", "button", "caption", "center", "cite", 
    "code", "col", "colgroup", "dd", "del", "dfn", "dir", "div", "dl", "dt", "em", 
    "fieldset", "font", "form", "frame", "frameset", "h1", "h2", "h3", "h4", 
    "h5", "h6", "head", "hr", "html", "i", "iframe", "img", "input", "ins", "isindex", 
    "kbd", "label", "legend", "li", "link", "map", "menu", "meta", "noframes", "noscript", 
    "object", "ol", "optgroup", "option", "p", "param", "pre", "q", "s", "samp", "script", "select", 
    "small", "span", "strike", "strong", "style", "sub", "sup", "table", "tbody", "td", "textarea", 
    "tfoot", "th", "thead", "title", "tr", "tt", "u", "ul", "var"
];


/*** EVENT WIRING ***/

NitrogenClass.prototype.$observe_event = function(anchor, path, type, func) {
    objs(path, anchor).bind(type, func);
}

/*** DYNAMIC UPDATING ***/

NitrogenClass.prototype.$update = function(anchor, path, html) {
    objs(path, anchor).html(html);
}

NitrogenClass.prototype.$replace = function(anchor, path, html) {
    objs(path, anchor).replaceWith(html);
}

NitrogenClass.prototype.$insert_top = function(anchor, path, html) {
    objs(path, anchor).prepend(html);
}

NitrogenClass.prototype.$insert_bottom = function(anchor, path, html) {
    objs(path, anchor).append(html);
}

NitrogenClass.prototype.$insert_before = function(anchor, path, html) {
    objs(path, anchor).before(html);
}

NitrogenClass.prototype.$insert_after = function(anchor, path, html) {
    objs(path, anchor).after(html);
}

NitrogenClass.prototype.$remove = function(anchor, path) {
    objs(path, anchor).remove();
}


/*** MISC ***/

NitrogenClass.prototype.$return_false = function(value, args) { 
    return false; 
}

NitrogenClass.prototype.$is_key_code = function(event, keyCode, shiftKey) {
    return (event && event.keyCode == keyCode && event.shiftKey == shiftKey);
}

NitrogenClass.prototype.$go_next = function(controlID) {
    var o = obj(controlID);
    if (o.focus) o.focus();
    if (o.select) o.select();
    if (o.click) o.click();
}

NitrogenClass.prototype.$disable_selection = function(element) {
    element.onselectstart = function() {
    return false;
    };
    element.unselectable = "on";
    element.style.MozUserSelect = "none";
    element.style.cursor = "default";
}

NitrogenClass.prototype.$set_value = function(anchor, element, value) {
    if (!element.id) element = objs(element);
    element.each(function(index, el) {
                     if (el.value != undefined) el.value = value;
                     else if (el.checked != undefined) el.checked = value;
                     else if (el.src != undefined) el.src = value;
                     else $(el).html(value);
                 });
}

NitrogenClass.prototype.$get_value = function(anchor, element) {
    if (!element.id) element = objs(element);
    el = element.get(0);
    if (el.value != undefined) return el.value;
    else if (el.checked != undefined) return el.checked;
    else if (el.src != undefined) return el.src;
    else return $(el).html();
}

NitrogenClass.prototype.$normalize_param = function(key, value) {
    // Create the key=value line to add.
    // Sometimes, the user will pass a bunch of params in the key field.
    var s = "";
    if (key) { s = key; }
    if (key && value) { s = key + "=" + value; }
    return key + "&" + value;
}

NitrogenClass.prototype.$encode_arguments_object = function(Obj) {
    if (! Bert) { alert("Bert.js library not included in template.") }
    var a = new Array();
    for (var i=0; i<Obj.length; i++) {
    a.push(Obj[i]);
    }
    var s = Bert.encode(a);
    return "args=" + this.$urlencode(s);
}

NitrogenClass.prototype.$urlencode = function(str) {
    return escape(str).replace(/\+/g,'%2B').replace(/%20/g, '+').replace(/\*/g, '%2A').replace(/\//g, '%2F').replace(/@/g, '%40');
}

/*** DATE PICKER ***/

NitrogenClass.prototype.$datepicker = function(pickerObj, pickerOptions) {
    jQuery(pickerObj).datepicker(pickerOptions);
}

/*** AUTOCOMPLETE TEXTBOX ***/
NitrogenClass.prototype.$autocomplete = function(path, autocompleteOptions, enterPostbackInfo, selectPostbackInfo) {
    var n = this;
    jQuery.extend(autocompleteOptions, {
        select: function(ev, ui) {
          var item = (ui.item) && '{"id":"'+ui.item.id+'","value":"'+ui.item.value+'"}' || '';
          n.$queue_event(null, selectPostbackInfo, "select_item="+n.$urlencode(item));
        },
        source: function(req, res) {
          n.$queue_event(null, enterPostbackInfo, "search_term="+req.term, {
              dataType: 'json',
              success: function(data) {
                 res(data);
              }
          });
        }
    });
    jQuery(path).autocomplete(autocompleteOptions);
}

/*** DRAG AND DROP ***/

NitrogenClass.prototype.$draggable = function(path, dragOptions, dragTag) {
    objs(path).each(function(index, el) {
          el.$drag_tag = dragTag;
        jQuery(el).draggable(dragOptions);
    });
}

NitrogenClass.prototype.$droppable = function(path, dropOptions, dropPostbackInfo) {
    var n = this;
    dropOptions.drop = function(ev, ui) {
        var dragItem = ui.draggable[0].$drag_tag;
        n.$queue_event(null, dropPostbackInfo, "drag_item=" + dragItem);
    };
    objs(path).each(function(index, el) {
          jQuery(el).droppable(dropOptions);
    });
}



/*** SORTING ***/

NitrogenClass.prototype.$sortitem = function(el, sortTag) {
    var sortItem = obj(el);
    sortItem.$sort_tag = sortTag;
    sortItem.$drag_tag = sortTag;
}

NitrogenClass.prototype.$sortblock = function(el, sortOptions, sortPostbackInfo) {
    var n = this;
    sortOptions.update = function() {
    var sortItems = "";
    for (var i=0; i<this.childNodes.length; i++) {
        var childNode = this.childNodes[i];
        if (sortItems != "") sortItems += ",";
        if (childNode.$sort_tag) sortItems += childNode.$sort_tag;
    }
    n.$queue_event(null, sortPostbackInfo, "sort_items=" + sortItems);
    };
    objs(el).sortable(sortOptions);
}

/*** transfer content of an alien elment into a nitrogen form
 * used in src/elements/other/element_recaptcha.erl
 * ***/
NitrogenClass.prototype.$from_alien = function(nativeID) {
    var input = $("input#" + nativeID).val();
    objs(nativeID).val(input);
};

var Nitrogen = new NitrogenClass();

var ws;
function addStatus(text){
    var date = new Date();
    document.getElementById('status').innerHTML =
    document.getElementById('status').innerHTML + "E> " + text + "<br/>";
}
function WSI(){
    if ("MozWebSocket" in window) { WebSocket = MozWebSocket; }
    if ("WebSocket" in window) {
        ws = new WebSocket("ws://192.168.1.103:8000/websocket");
        ws.onopen = function() {
            addStatus("websocket connected!");
//            ws.send("hello server!"); 
            addStatus("sent message to server: 'hello server'!");
        };
        ws.onmessage = function (evt) { // EVENT DISPATCHER
            var receivedMsg = evt.data;
            addStatus("server sent the following: '" + receivedMsg + "'");
        };
        ws.onclose = function() {
            addStatus("websocket was closed");
        };
    } else {
        addStatus("sorry, your browser does not support websockets.");
    }
}

var page = document;
WSI();
Nitrogen.$event_loop();
