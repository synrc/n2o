-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-define(CTX, (wf_context:context())).
-define(REQ, (wf_context:context())#cx.req).

-define(HANDLER_API, [init/2, finish/2]).
-define(FAULTER_API, [error_page/2]).
-define(ROUTING_API, [init/2, finish/2]).
-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2, clear/0]).
-define(PICKLES_API, [pickle/1, depickle/1]).
-define(MESSAGE_API, [send/2, reg/1, reg/2, unreg/1]).

-record(handler, {name, module, config, state}).
-record(cx,      {handlers, actions, req, module, path, session, params, form, state=[]}).
-record(ev,      {module, msg, trigger, name :: api_event | control_event | event | atom() }).

-define(DEFAULT_BASE, {?ELEMENT_BASE(undefined)}).
-define(DEFAULT_BASE_TAG(Tag), {?ELEMENT_BASE(undefined,Tag,undefined)}).
-define(ELEMENT_BASE(Module), ?ELEMENT_BASE(Module,undefined,undefined)).
-define(ELEMENT_BASE(Module,Tag,Delegate),
        ancestor=element, id, module=Module, delegate=Delegate, validation=[], actions, class=[], style=[], source=[], onmouseover, onkeypress, onchange, onkeyup, onkeydown, onclick,
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

% HTML Document meta
-record(base,       {?ELEMENT_BASE(element_meta_base), href, target}).
-record(head,       ?DEFAULT_BASE).
-record(meta_link,       {?ELEMENT_BASE(element_meta_link), href, hreflang, media, rel, sizes, type}).
-record(meta,       {?ELEMENT_BASE(element_meta), charset, content, http_equiv, name, type}).
-record(style,       {?ELEMENT_BASE(element_style), media, scoped, type}).
-record(title,       ?DEFAULT_BASE).

% HTML Edits
-record('del',       {?ELEMENT_BASE(element_del), cite, datetime}).
-record(ins,       {?ELEMENT_BASE(element_ins), cite, datetime}).

% HTML Embedded
-record(area,       {?ELEMENT_BASE(element_area), alt, coords, href, hreflang, media, target, rel, shape, type}).
-record(audio,       {?ELEMENT_BASE(element_audio), autoplay, controls, loop, mediagroup, muted, preload, src, width}).
-record(canvas,       {?ELEMENT_BASE(element_canvas), height, width}).
-record(embed,       {?ELEMENT_BASE(element_embed), height, src, type, width}).
-record(iframe,       {?ELEMENT_BASE(element_iframe), height, name, sandbox, seamless, src, srcdoc, width}).
-record(image,       {?ELEMENT_BASE(element_image), alt, height, ismap, src, usemap, width, image}).
-record(map,       {?ELEMENT_BASE(element_map), name}).
-record(object,       {?ELEMENT_BASE(element_object), data, form, height, name, type, usemap, width}).
-record(param,       {?ELEMENT_BASE(element_param), name, value}).
-record(source,       {?ELEMENT_BASE(element_source), media, src, type}).
-record(track,       {?ELEMENT_BASE(element_track), default, kind, label, src, srclang}).
-record(video,       {?ELEMENT_BASE(element_video), autoplay, controls, height, loop, mediagroup, muted, poster, preload, src, width}).

% HTML Form
-record(button,       {?ELEMENT_BASE(element_button), autofocus, disabled, form, formaction, formenctype, formmethod, formtarget, formnovalidate, name, type= <<"button">>, value, postback}).
-record(datalist,       ?DEFAULT_BASE).
-record(fieldset,       {?ELEMENT_BASE(element_fieldset), disabled, form, name, legend}).
-record(form,       {?ELEMENT_BASE(element_form), accept_charset, action, autocomplete, enctype, method, name, novalidate, target}).
-record(keygen,       {?ELEMENT_BASE(element_keygen), autofocus, challenge, disabled, form, keytype, name, postback}).
-record(legend,       ?DEFAULT_BASE).
-record(label,       {?ELEMENT_BASE(element_label), for, form, postback}).
-record(meter,       {?ELEMENT_BASE(element_meter), high, low, max, min, optimum, value, postback}).
-record(optgroup,       {?ELEMENT_BASE(element_select), disabled, label}).
-record(option,       {?ELEMENT_BASE(element_select), disabled, label, selected=false, value, postback}).
-record(output,       {?ELEMENT_BASE(element_output), for, form, name, postback}).
-record(progress,       {?ELEMENT_BASE(element_progress), max, value, postback}).
-record(select,       {?ELEMENT_BASE(element_select), autofocus, disabled, form, multiple, name, required, size, postback}).
-record(textarea,       {?ELEMENT_BASE(element_textarea), autofocus, cols, dirname, disabled, form, maxlength, name, placeholder, readonly, required, rows, wrap, postback, value}).

% HTML Form inputs
-record(input,       {?ELEMENT_BASE(element_input),  autofocus, disabled, form, name, value, postback, type=[], multiple}).
-record(input_button,       {?ELEMENT_BASE(element_input_button),  autofocus, disabled, form, name, value, postback}).
-record(checkbox,           {?ELEMENT_BASE(element_checkbox),  autofocus, checked=false, disabled, form, name, required, value, postback}).
-record(color,           {?ELEMENT_BASE(element_color),  autocomplete, autofocus, disabled, form, list, name, value, postback}).
-record(date,           {?ELEMENT_BASE(element_date),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
-record(datetime,           {?ELEMENT_BASE(element_datetime),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
-record(datetime_local,           {?ELEMENT_BASE(element_datetime_local),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
-record(email,           {?ELEMENT_BASE(element_email),  autocomplete, autofocus, disabled, form, list, maxlength, multiple, name, pattern, placeholder, readonly, required, size, value, postback}).
-record(file,           {?ELEMENT_BASE(element_file),  accept, autofocus, disabled, form, multiple, name, required, postback}).
-record(hidden,           {?ELEMENT_BASE(element_hidden),  disabled, form, name, value, postback, html_name}).
-record(input_image,           {?ELEMENT_BASE(element_input_image),  alt, autofocus, disabled, form, formaction, formenctype, formmethod, formnovalue, formtarget, height, name, src, width, postback}).
-record(month,              {?ELEMENT_BASE(element_month),  alt, autocomplite, autofocus, disabled, form, list, min, max, name, readonly, required, step, value, postback}).
-record(number,              {?ELEMENT_BASE(element_number),  autocomplete, autofocus, disabled, form, list, max, min, name, placeholder, readonly, required, step, value, postback}).
-record(password,              {?ELEMENT_BASE(element_password),  autocomplete, autofocus, disabled, form, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
-record(radio,              {?ELEMENT_BASE(element_radio),  autofocus, checked, disabled, form, name, required, value, postback, html_name}).
-record(range,              {?ELEMENT_BASE(element_range),  autocomplete, autofocus, disabled, form, list, max=100, min=0, name, step=1, value, postback}).
-record(reset,              {?ELEMENT_BASE(element_reset),  autofocus, disabled, form, name, value, postback}).
-record(search,              {?ELEMENT_BASE(element_search),  autocomplete, autofocus, dirname, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
-record(submit,              {?ELEMENT_BASE(element_submit),  autofocus, disabled, form, formaction, formenctype, formmethod, formnovalidate, formtarget, name, value, postback, click}).
-record(tel,              {?ELEMENT_BASE(element_tel),  autocomplete, autofocus, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
-record(textbox,              {?ELEMENT_BASE(element_textbox),  autocomplete, autofocus, dirname, disabled, form, list, maxlength, name, pattern, placeholder, readony, required, size, value, postback}).
-record(input_time,              {?ELEMENT_BASE(element_input_time),  autocomplete, autofocus, disabled, form, list, max, min, name, step, readonly, required, value, postback}).
-record(url,              {?ELEMENT_BASE(element_url),  autocomplete, autofocus, disabled, form, list, maxlength, name, pattern, placeholder, readonly, required, size, value, postback}).
-record(week,              {?ELEMENT_BASE(element_week),  autocomplete, autofocus, disabled, form, list, max, min, name, readonly, required, step, value, postback}).

% HTML Interactive
-record(command,       {?ELEMENT_BASE(element_command),  checked, disabled, icon, label, radiogroup, type= <<"command">>}).
-record(details,       {?ELEMENT_BASE(element_details),  open}).
-record(menu,       {?ELEMENT_BASE(element_menu),  label, type}).
-record(summary,       ?DEFAULT_BASE).

% HTML Grouping content
-record(blockquote,		{?ELEMENT_BASE(element_blockquote),  cite}).
-record(br,       		?DEFAULT_BASE).
-record(dd,       		?DEFAULT_BASE).
-record('div',      	?DEFAULT_BASE_TAG(<<"div">>)).
-record(dl,       		?DEFAULT_BASE).
-record(dt,       		?DEFAULT_BASE).
-record(figcaption,		?DEFAULT_BASE).
-record(figure,       	?DEFAULT_BASE).
-record(hr,       		?DEFAULT_BASE).
-record(li,             {?ELEMENT_BASE(element_li),  value}).
-record(ol,             ?DEFAULT_BASE).
-record(p,       		?DEFAULT_BASE).
-record(panel,          ?DEFAULT_BASE_TAG(<<"div">>)).
-record(pre,       		?DEFAULT_BASE).
-record(ul,       		?DEFAULT_BASE).

% HTML Root
-record(html,			{?ELEMENT_BASE(element_html), manifest}).

% HTML Scripting
-record(script,			{?ELEMENT_BASE(element_script),  async, charset, defer, src, type}).
-record(noscript,      	?DEFAULT_BASE).

% HTML Sections
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

% HTML Table
-record(caption,       	?DEFAULT_BASE).
-record(col,            {?ELEMENT_BASE(element_col),  span}).
-record(colgroup,       {?ELEMENT_BASE(element_colgroup), col, span}).
-record(table,          {?ELEMENT_BASE(element_table),  caption, colgroup, border, footer, header}).
-record(tbody,          ?DEFAULT_BASE).
-record(td, 			{?ELEMENT_BASE(element_td), colspan=1, headers, rowspan=1, scope}).
-record(tfoot,       	?DEFAULT_BASE).
-record(th, 			{?ELEMENT_BASE(element_th), colspan=1, headers, rowspan=1, scope}).
-record(thead,       	?DEFAULT_BASE).
-record(tr, 			{?ELEMENT_BASE(element_tr), cells, postback}).

% HTML Text-level semantics
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
