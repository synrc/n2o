-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-define(CTX, (wf_context:context())).
-define(REQ, (wf_context:context())#context.req).

-define(HANDLER_API, [init/2, finish/2]).
-define(FAULTER_API, [error_page/2]).
-define(ROUTING_API, [init/2, finish/2]).
-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2, clear/0]).
-define(PICKLES_API, [pickle/1, depickle/1]).
-define(MESSAGE_API, [send/2, reg/1, reg/2]).

-record(handler, {name, module, config, state}).
-record(context, {handlers, actions, req, module, path, session, params}).
-record(ev,      {module, payload, trigger, name :: api_event | control_event | event | atom() }).

-define(DEFAULT_BASE, {?ELEMENT_BASE(undefined)}).
-define(DEFAULT_BASE_TAG(Tag), {?ELEMENT_BASE(undefined,Tag,undefined)}).
-define(ELEMENT_BASE(Module), ?ELEMENT_BASE(Module,undefined,undefined)).
-define(ELEMENT_BASE(Module,Tag,Delegate),
        ancestor=element, module=Module, delegate=Delegate, id, actions, class=[], style=[], source=[],
        data_fields=[], aria_states=[], body, role, tabindex, show_if=true, html_tag=Tag, title, accesskey, contenteditable, contextmenu, dir, draggable, dropzone, hidden, lang, spellcheck, translate, onafterprint, onbeforeprint, onbeforeunload, onblur, onerror, onfocus, onhashchange, onload, onmessage, onoffline, ononline, onpagehide, onpageshow, onpopstate, onresize, onstorage, onunload).
-define(ACTION_BASE(Module),
        ancestor=action, trigger, target, module=Module, actions, source=[]).
-define(CTRL_BASE(Module), ?ELEMENT_BASE(Module,undefined,Module)).

-record(element, {?ELEMENT_BASE(undefined)}).
-record(literal, {?ELEMENT_BASE(element_literal)}).
-record(dtl, {?ELEMENT_BASE(element_dtl), file="index", bindings=[], app=web, folder="priv/templates", ext="html", bind_script=true }).
-record(list, {?ELEMENT_BASE(element_list), numbered=false }).
-record(dropdown, {?ELEMENT_BASE(element_dropdown), options, postback, value, multiple=false, disabled=false, name}).
-record(radiogroup, {?ELEMENT_BASE(element_radiogroup)}).
-record(spinner, {?ELEMENT_BASE(element_spinner), image="/nitrogen/spinner.gif"}).

%%%%%%%%%%%%%%%%%%%%%%
% HTML Document meta %
%%%%%%%%%%%%%%%%%%%%%%
% base spec: href, target
-record(base,       {?ELEMENT_BASE(element_meta_base), href, target}).
-record(head,       ?DEFAULT_BASE).
% link spec: href, hreflang, media, rel, sizes, type
-record(meta_link,       {?ELEMENT_BASE(element_meta_link), href, hreflang, media, rel, sizes, type}).
% meta spec: charset, content, http-equiv, name
% meta spec (n2o): type
-record(meta,       {?ELEMENT_BASE(element_meta), charset, content, http_equiv, name, type}).
% style spec: media, scoped, type
-record(style,       {?ELEMENT_BASE(element_style), media, scoped, type}).
-record(title,       ?DEFAULT_BASE).

%%%%%%%%%%%%%%
% HTML Edits %
%%%%%%%%%%%%%%
% del spec: cite, datetime
-record('del',       {?ELEMENT_BASE(element_del), cite, datetime}).
% ins spec: cite, datetime
-record(ins,       {?ELEMENT_BASE(element_ins), cite, datetime}).

%%%%%%%%%%%%%%%%%
% HTML Embedded %
%%%%%%%%%%%%%%%%%
% area spec: alt, coords, href, hreflang, media, target, rel, shape, type
-record(area,       {?ELEMENT_BASE(element_area), alt, coords, href, hreflang, media, target, rel, shape, type}).
% audio spec: autoplay, controls, loop, mediagroup, muted, preload, src, width
-record(audio,       {?ELEMENT_BASE(element_audio), autoplay, controls, loop, mediagroup, muted, preload, src, width}).
% canvas spec: height, width
-record(canvas,       {?ELEMENT_BASE(element_canvas), height, width}).
% embed spec: height, src, type, width
-record(embed,       {?ELEMENT_BASE(element_embed), height, src, type, width}).
% iframe spec: height, name, sandbox, seamless, src, srcdoc, width
-record(iframe,       {?ELEMENT_BASE(element_iframe), height, name, sandbox, seamless, src, srcdoc, width}).
% img spec: alt, height, ismap, src, usemap, width
% img n2o spec: image
-record(image,       {?ELEMENT_BASE(element_image), alt, height, ismap, src, usemap, width, image}).
% map spec: name
-record(map,       {?ELEMENT_BASE(element_map), name}).
% object spec: data, form, height, name, type, usemap, width
-record(object,       {?ELEMENT_BASE(element_object), data, form, height, name, type, usemap, width}).
% param spec: name, value
-record(param,       {?ELEMENT_BASE(element_param), name, value}).
% source spec: media, src, type
-record(source,       {?ELEMENT_BASE(element_source), media, src, type}).
% track spec: default, kind, label, src, srclang 
-record(track,       {?ELEMENT_BASE(element_track), default, kind, label, src, srclang}).
% video spec: autoplay, controls, height, loop, mediagroup, muted, poster, preload, src, width
-record(video,       {?ELEMENT_BASE(element_video), autoplay, controls, height, loop, mediagroup, muted, poster, preload, src, width}).

%%%%%%%%%%%%%
% HTML Form %
%%%%%%%%%%%%%
% button spec: autofocus, disabled, form, formaction, formenctype, formmethod, formtarget, formnovalidate, name, type, value
-record(button,       {?ELEMENT_BASE(element_button), autofocus, disabled, form, formaction, formenctype, formmethod, formtarget, formnovalidate, name, type= <<"button">>, value, postback}).
-record(datalist,       ?DEFAULT_BASE).
% fieldset spec: disabled, form, name
-record(fieldset,       {?ELEMENT_BASE(element_fieldset), disabled, form, name, legend}).
% form spec: accept-charset, action, autocomplete, enctype, method, name, novalidate, target
-record(form,       {?ELEMENT_BASE(element_form), accept_charset, action, autocomplete, enctype, method, name, novalidate, target}).
% keygen spec: autofocus, challenge, disabled, form ,keytype, name
-record(keygen,       {?ELEMENT_BASE(element_keygen), autofocus, challenge, disabled, form, keytype, name, postback}).
-record(legend,       ?DEFAULT_BASE).
% label spec: for, form
-record(label,       {?ELEMENT_BASE(element_label), for, form, postback}).
% meter spec: high, low, max, min, optimum, value
-record(meter,       {?ELEMENT_BASE(element_meter), high, low, max, min, optimum, value, postback}).
% optgroup spec: disabled, label
-record(optgroup,       {?ELEMENT_BASE(element_select), disabled, label}).
% option spec: disabled, label, selected, value
-record(option,       {?ELEMENT_BASE(element_select), disabled, label, selected=false, value, postback}).
% output spec: for, form, name
-record(output,       {?ELEMENT_BASE(element_output), for, form, name, postback}).
% progress spec: max, value
-record(progress,       {?ELEMENT_BASE(element_progress), max, value, postback}).
% select spec: autofocus, disabled, form, multiple, name, required, size
-record(select,       {?ELEMENT_BASE(element_select), autofocus, disabled, form, multiple, name, required, size, postback}).
% textarea spec: autofocus, cols, dirname, disabled, form, maxlength, name, placeholder, readonly, required, rows, wrap
% textarea n2o spec: value
-record(textarea,       {?ELEMENT_BASE(element_textarea), autofocus, cols, dirname, disabled, form, maxlength, name, placeholder, readonly, required, rows, wrap, postback, value}).

%%%%%%%%%%%%%%%%%%%%
% HTML Form inputs %
%%%%%%%%%%%%%%%%%%%%

% button spec: autofocus, disabled, form, name, value
-record(input_button,       {?ELEMENT_BASE(element_input_button),  autofocus, disabled, form, name, value, postback}).
% checkbox spec: autofocus, checked, disabled, form, name, required, value
-record(checkbox,           {?ELEMENT_BASE(element_checkbox),  autofocus, checked=false, disabled, form, name, required, value, postback}).
% color spec: autocomplete, autofocus, disabled, form, list, name, value
-record(color,           {?ELEMENT_BASE(element_color),  autocomplete, autofocus, disabled, form, list, name, value, postback}).
% date spec: autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value
-record(date,           {?ELEMENT_BASE(element_date),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
% datetime spec: autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value
-record(datetime,           {?ELEMENT_BASE(element_datetime),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
% datetime_local spec: autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value
-record(datetime_local,           {?ELEMENT_BASE(element_datetime_local),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
% email spec: autocomplete, autofocus, disabled, form, list, maxlength, multiple, name, pattern, placeholder, readonly, required, size, value
-record(email,           {?ELEMENT_BASE(element_email),  autocomplete, autofocus, disabled, form, list, maxlength, multiple, name, pattern, placeholder, readonly, required, size, value, postback}).
% file spec: accept, autofocus, disabled, form, multiple, name, required
-record(file,           {?ELEMENT_BASE(element_file),  accept, autofocus, disabled, form, multiple, name, required, postback}).
% hidden spec: disabled, form, name, value
% hidden n2o spec: html_name
-record(hidden,           {?ELEMENT_BASE(element_hidden),  disabled, form, name, value, postback, html_name}).
% image spec: alt, autofocus, disabled, form, formaction, formenctype, formmethod, formnovalue, formtarget, height, name, src, width
-record(input_image,           {?ELEMENT_BASE(element_input_image),  alt, autofocus, disabled, form, formaction, formenctype, formmethod, formnovalue, formtarget, height, name, src, width, postback}).
% month spec: alt, autocomplite, autofocus, disabled, form, list, min, max, name, readonly, required, step, value
-record(month,              {?ELEMENT_BASE(element_month),  alt, autocomplite, autofocus, disabled, form, list, min, max, name, readonly, required, step, value, postback}).
% number spec: autocomplete, autofocus, disabled, form, list, max, min, name, placeholder, readonly, required, step, value
-record(number,              {?ELEMENT_BASE(element_number),  autocomplete, autofocus, disabled, form, list, max, min, name, placeholder, readonly, required, step, value, postback}).
% password spec: autocomplete, autofocus, disabled, form, maxlength, name, pattern, placeholder, readonly, required, size, value
-record(password,              {?ELEMENT_BASE(element_password),  autocomplete, autofocus, disabled, form, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
% radio spec: autofocus, checked, disabled, form, name, required, value
% radio n2o spec: html_name
-record(radio,              {?ELEMENT_BASE(element_radio),  autofocus, checked, disabled, form, name, required, value, postback, html_name}).
% range spec: autocomplete, autofocus, disabled, form, list, max, min, name, step, value
-record(range,              {?ELEMENT_BASE(element_range),  autocomplete, autofocus, disabled, form, list, max=100, min=0, name, step=1, value, postback}).
% reset spec: autofocus, disabled, form, name, value
-record(reset,              {?ELEMENT_BASE(element_reset),  autofocus, disabled, form, name, value, postback}).
% search spec: autocomplete, autofocus, dirname, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value
-record(search,              {?ELEMENT_BASE(element_search),  autocomplete, autofocus, dirname, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
% submit spec: autofocus, disabled, form, formaction, formenctype, formmethod, formnovalidate, formtarget, name, value
% submit n2o spec: click
-record(submit,              {?ELEMENT_BASE(element_submit),  autofocus, disabled, form, formaction, formenctype, formmethod, formnovalidate, formtarget, name, value, postback, click}).
% tel spec: autocomplete, autofocus, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value
-record(tel,              {?ELEMENT_BASE(element_tel),  autocomplete, autofocus, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
% text spec: autocomplete, autofocus, dirname, disabled, form, list, maxlength, name, pattern, placeholder, readony, required, size, value
-record(textbox,              {?ELEMENT_BASE(element_textbox),  autocomplete, autofocus, dirname, disabled, form, list, maxlength, name, pattern, placeholder, readony, required, size, value, postback}).
% time spec: autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value
-record(input_time,              {?ELEMENT_BASE(element_input_time),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
% url spec: autocomplete, autofocus, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value
-record(url,              {?ELEMENT_BASE(element_url),  autocomplete, autofocus, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
% week spec: autocomplete, autofocus, disabled, form, list, max, min, name, readonly, required, step, value
-record(week,              {?ELEMENT_BASE(element_week),  autocomplete, autofocus, disabled, form, list, max, min, name, readonly, required, step, value, postback}).

%%%%%%%%%%%%%%%%%%%%
% HTML Interactive %
%%%%%%%%%%%%%%%%%%%%
% command spec: checked, disabled, icon, label, radiogroup, type
-record(command,       {?ELEMENT_BASE(element_command),  checked, disabled, icon, label, radiogroup, type= <<"command">>}).
% details spec: open
-record(details,       {?ELEMENT_BASE(element_details),  open}).
% menu spec: label, type
-record(menu,       {?ELEMENT_BASE(element_menu),  label, type}).
-record(summary,       ?DEFAULT_BASE).

%%%%%%%%%%%%%%%%%%%%%%%%%
% HTML Grouping content %
%%%%%%%%%%%%%%%%%%%%%%%%%

% blockquote spec: cite
-record(blockquote,		{?ELEMENT_BASE(element_blockquote),  cite}).
-record(br,       		?DEFAULT_BASE).
-record(dd,       		?DEFAULT_BASE).
-record('div',      	?DEFAULT_BASE_TAG(<<"div">>)).
-record(dl,       		?DEFAULT_BASE).
-record(dt,       		?DEFAULT_BASE).
-record(figcaption,		?DEFAULT_BASE).
-record(figure,       	?DEFAULT_BASE).
-record(hr,       		?DEFAULT_BASE).
% li spec: value
-record(li,             {?ELEMENT_BASE(element_li),  value}).
% ol spec: reversed, start, type
-record(ol,             {?ELEMENT_BASE(element_ol),  reversed, start, type}).
-record(p,       		?DEFAULT_BASE).
-record(panel,          ?DEFAULT_BASE_TAG(<<"div">>)).
-record(pre,       		?DEFAULT_BASE).
-record(ul,       		?DEFAULT_BASE).

%%%%%%%%%%%%%
% HTML Root %
%%%%%%%%%%%%%

% html spec: manifest
-record(html,			{?ELEMENT_BASE(element_html), manifest}).

%%%%%%%%%%%%%%%%%%
% HTML Scripting %
%%%%%%%%%%%%%%%%%%

% script spec: async, charset, defer, src, type
-record(script,			{?ELEMENT_BASE(element_script),  async, charset, defer, src, type}).
-record(noscript,      	?DEFAULT_BASE).

%%%%%%%%%%%%%%%%%
% HTML Sections %
%%%%%%%%%%%%%%%%%

% body spec: onafterprint, onbeforeprint, onbeforeunload, onblur, onerror, onfocus, onhashchange, onload, onmessage, onoffline, ononline, onpagehide, onpageshow, onpopstate, onresize, onstorage, onunload
-record(body,       	?DEFAULT_BASE).
-record(section,    	?DEFAULT_BASE).
-record(nav,        	?DEFAULT_BASE).
-record(article,    	?DEFAULT_BASE).
-record(aside,      	?DEFAULT_BASE).
-record(h1,         	?DEFAULT_BASE).
-record(h2,         	?DEFAULT_BASE).
-record(h3,         	?DEFAULT_BASE).
-record(h4,         	?DEFAULT_BASE).
-record(h5,         	?DEFAULT_BASE).
-record(h6,         	?DEFAULT_BASE).
-record(header,     	?DEFAULT_BASE).
-record(hgroup,     	?DEFAULT_BASE).
-record(footer,     	?DEFAULT_BASE).
-record(address,    	?DEFAULT_BASE).
-record(main,       	?DEFAULT_BASE).

%%%%%%%%%%%%%%
% HTML Table %
%%%%%%%%%%%%%%

-record(caption,       	?DEFAULT_BASE).
% col spec: span
-record(col,            {?ELEMENT_BASE(element_col),  span}).
% colgroup spec: span + col
-record(colgroup,       {?ELEMENT_BASE(element_colgroup), col, span}).
% table spec: border + colgroup, caption + n2o: header, footer
-record(table,          {?ELEMENT_BASE(element_table),  caption, colgroup, border, footer, header}).
-record(tbody,          ?DEFAULT_BASE).
% td spec: headers, colspan, rowspan
% td n2o spec: scope
-record(td, 			{?ELEMENT_BASE(element_td), colspan=1, headers, rowspan=1, scope}).
-record(tfoot,       	?DEFAULT_BASE).
% th spec: colspan, headers, rowspan, scope
-record(th, 			{?ELEMENT_BASE(element_th), colspan=1, headers, rowspan=1, scope}).
-record(thead,       	?DEFAULT_BASE).
% tr n2o spec: cells, postback
-record(tr, 			{?ELEMENT_BASE(element_tr), cells, postback}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HTML Text-level semantics %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a spec: href, hreflang, media, rel, target, type
% a n2o spec: url, download, name, postback
-record(link,           {?ELEMENT_BASE(element_link),  href, hreflang, media, rel, target, type, url="javascript:void(0);", download, name, postback}).
-record(abbr,       	?DEFAULT_BASE).
-record(b,       		?DEFAULT_BASE).
-record(bdi,       		?DEFAULT_BASE).
-record(bdo,       		?DEFAULT_BASE).
-record(cite,       	?DEFAULT_BASE).
-record(code,       	?DEFAULT_BASE).
-record(dfn,       		?DEFAULT_BASE).
-record(em,       		?DEFAULT_BASE).
-record(i,       		?DEFAULT_BASE).
-record(kbd,       		?DEFAULT_BASE).
-record(mark,       	?DEFAULT_BASE).
% q spec: cite
-record(q,              {?ELEMENT_BASE(element_q),  cite}).
-record(rt,       		?DEFAULT_BASE).
-record(rp,       		?DEFAULT_BASE).
-record(ruby,       	?DEFAULT_BASE).
-record(s,       		?DEFAULT_BASE).
-record(samp,       	?DEFAULT_BASE).
-record(small,       	?DEFAULT_BASE).
-record(span,       	?DEFAULT_BASE).
-record(strong,       	?DEFAULT_BASE).
-record(sub,       		?DEFAULT_BASE).
-record(sup,       		?DEFAULT_BASE).
% time spec: datetime
-record(time,           {?ELEMENT_BASE(element_time),  datetime}).
-record(u,       		?DEFAULT_BASE).
-record(var,       		?DEFAULT_BASE).

% Actions
-record(action,  {?ACTION_BASE(undefined)}).
-record(wire,    {?ACTION_BASE(action_wire)}).
-record(api,     {?ACTION_BASE(action_api), name, tag, delegate }).
-record(event,   {?ACTION_BASE(action_event), type=default, postback, delegate}).
-record(alert,   {?ACTION_BASE(action_alert), text}).
-record(confirm, {?ACTION_BASE(action_confirm), text, postback, delegate}).
-record(jq,      {?ACTION_BASE(action_jq), property, method, args=[], right, format="~s"}).

%Binary messaging to browser
-record(binary, {
    id = 0      :: integer(),   % 4 bytes unsigned
    type = 0    :: integer(),   % 1 byte unsigned
    app = 0     :: integer(),   % 1 byte unsigned
    version = 0 :: integer(),   % 1 byte unsigned
    from = 0    :: integer(),   % 4 bytes unsigned
    to = 0      :: integer(),   % 4 bytes unsigned
    user1 = 0   :: integer(),   % 8 bytes signed float, user defined, e.g.: -define(TIMESTAMP, user1).
    user2 = 0   :: integer(),   % 8 bytes signed float, user defined, e.g.: -define(EXPIRES, user2).
    meta = <<>> :: binary(),    % binary
    data = <<>> :: binary() }). % binary
-endif.
