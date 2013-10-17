-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-define(CTX, (wf_context:context())).
-define(REQ, (wf_context:context())#context.req).
-define(HANDLER_API, [init/2, finish/2]).
-define(ROUTING_API, [init/2, finish/2]).
-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2]).
-define(PICKLES_API, [pickle/1, depickle/1]).
-define(MESSAGE_API, [send/2, reg/1]).

-record(handler, {name, module, config, state}).
-record(context, {handlers, actions, req, module, path, session, params}).
-record(ev,      {module, payload, trigger, name :: api_event | control_event | event | atom() }).

-define(DEFAULT_BASE, {?ELEMENT_BASE(undefined)}).
-define(DEFAULT_BASE_TAG(Tag), {?ELEMENT_BASE_TAG(undefined,Tag)}).
-define(ELEMENT_BASE(Module), ?ELEMENT_BASE_TAG(Module,undefined)).
-define(ELEMENT_BASE_TAG(Module,Tag),
        ancestor=element, module=Module, id, actions, class=[], style=[], source=[],
        data_fields=[], aria_states=[], body, role, tabindex, show_if=true, html_tag=Tag, title).

-define(ACTION_BASE(Module),
        ancestor=action, trigger, target, module=Module, actions, source=[]).

-record(element, {?ELEMENT_BASE(undefined)}).
-record(range, {?ELEMENT_BASE(element_range), min=0, max=100, step=1, value=0, next, postback, delegate}).
-record(literal, {?ELEMENT_BASE(element_literal)}).
-record(dtl, {?ELEMENT_BASE(element_dtl), file="index", bindings=[], app=web, folder="priv/templates", ext="html", bind_script=true }).
-record(list, {?ELEMENT_BASE(element_list), numbered=false }).
-record(label, {?ELEMENT_BASE(element_label), for=""}).
-record(link, {?ELEMENT_BASE(element_link), target, url="javascript:void(0);", postback, delegate, name}).
-record(submit, {?ELEMENT_BASE(element_submit), click, postback, delegate}).
-record(button, {?ELEMENT_BASE(element_button), type= <<"button">>, name, value, postback, delegate, disabled}).
-record(textbox, {?ELEMENT_BASE(element_textbox), value, disabled, maxlength="", placeholder="", next, postback, delegate, name}).
-record(hidden, {?ELEMENT_BASE(element_hidden), value, html_name, disabled=false}).
-record(textarea, {?ELEMENT_BASE(element_textarea), placeholder, name, cols, rows, value}).
-record(optgroup, {?ELEMENT_BASE(element_select), label, disabled}).
-record(option, {?ELEMENT_BASE(element_select), label, value, selected=false, disabled}).
-record(dropdown, {?ELEMENT_BASE(element_dropdown), options, postback, delegate, value, multiple=false, disabled=false, name}).
-record(select, {?ELEMENT_BASE(element_select), disabled, multiple, name, size, postback, delegate}).
-record(checkbox, {?ELEMENT_BASE(element_checkbox), checked=false, value="on", postback, delegate, name}).
-record(radiogroup, {?ELEMENT_BASE(element_radiogroup)}).
-record(radio, {?ELEMENT_BASE(element_radio), value, name, checked=false, postback, delegate, html_name}).
-record(password, {?ELEMENT_BASE(element_password), value, maxlength="", placeholder= <<"password">>, next, postback, delegate, name}).
-record(spinner, {?ELEMENT_BASE(element_spinner), image="/nitrogen/spinner.gif"}).
-record(image, {?ELEMENT_BASE(element_image), image="", alt="", width, height}).
-record(table, {?ELEMENT_BASE(element_table), header, footer}).
-record(td, {?ELEMENT_BASE(element_td), colspan=1, rowspan=1, scope}).
-record(th, {?ELEMENT_BASE(element_th), colspan=1, rowspan=1, scope}).
-record(tr, {?ELEMENT_BASE(element_tr), cells, postback, delegate}).
-record(file, {?ELEMENT_BASE(element_file), file}).
-record(blockquote, {?ELEMENT_BASE(element_blockquote), cite}).

% Sections
-record(body,       ?DEFAULT_BASE).
-record(section,    ?DEFAULT_BASE).
-record(nav,        ?DEFAULT_BASE).
-record(article,    ?DEFAULT_BASE).
-record(aside,      ?DEFAULT_BASE).
-record(h1,         ?DEFAULT_BASE).
-record(h2,         ?DEFAULT_BASE).
-record(h3,         ?DEFAULT_BASE).
-record(h4,         ?DEFAULT_BASE).
-record(h5,         ?DEFAULT_BASE).
-record(h6,         ?DEFAULT_BASE).
-record(header,     ?DEFAULT_BASE).
-record(footer,     ?DEFAULT_BASE).
-record(address,    ?DEFAULT_BASE).
-record(main,       ?DEFAULT_BASE).

% Grouping
-record(p,          ?DEFAULT_BASE).
-record(hr,         ?DEFAULT_BASE).
-record(pre,        ?DEFAULT_BASE).
-record(ul,         ?DEFAULT_BASE).
-record(li,         ?DEFAULT_BASE).
-record(dl,         ?DEFAULT_BASE).
-record(dt,         ?DEFAULT_BASE).
-record(dd,         ?DEFAULT_BASE).
-record(hgroup,     ?DEFAULT_BASE).
-record(figure,     ?DEFAULT_BASE).
-record(figcaption, ?DEFAULT_BASE).
-record(panel,      ?DEFAULT_BASE_TAG(<<"div">>)).
-record('div',      ?DEFAULT_BASE_TAG(<<"div">>)).

% Tables
-record(tbody,      ?DEFAULT_BASE).

% Forms
-record(fieldset,   ?DEFAULT_BASE).
-record(legend,     ?DEFAULT_BASE).

% Text
-record(br,         ?DEFAULT_BASE).
-record(i,          ?DEFAULT_BASE).
-record(b,          ?DEFAULT_BASE).
-record(strong,     ?DEFAULT_BASE).
-record(em,         ?DEFAULT_BASE).
-record(small,      ?DEFAULT_BASE).
-record(span,       ?DEFAULT_BASE).
-record(mark,       ?DEFAULT_BASE).
-record(abbr,       ?DEFAULT_BASE).

% Embedding
-record(canvas,     ?DEFAULT_BASE).

% Twitter Bootstrap Elements
-record(carousel, {?ELEMENT_BASE(element_carousel), interval=5000, pause= <<"hover">>, start=0, indicators=true, items=[], caption=[]}).
-record(accordion, {?ELEMENT_BASE(element_accordion), items=[], nav_stacked=false}).
-record(slider, {?ELEMENT_BASE(element_slider), min, max, step, orientation, value, selection, tooltip, handle, formater}).

% Synrc Elements
-record(rtable, {?ELEMENT_BASE(element_rtable), rows=[], delegate, postback}).
-record(upload, {?ELEMENT_BASE(element_upload), name, value, delegate_query, delegate, delegate_api, root=code:priv_dir(n2o), dir="", post_write, post_target, img_tool, preview=false, size=[{200,200}]}).
-record(textboxlist, {?ELEMENT_BASE(element_textboxlist), placeholder="", delegate, postback, unique=true, values=[], autocomplete=true, queryRemote=true, onlyFromValues=true, minLenght=1}).
-record(htmlbox, {?ELEMENT_BASE(element_htmlbox), html="", script_url="static/tinymce/tinymce.min.js", theme="n2o", delegate, delegate_api, toolbar_class, toolbar_script, root=code:priv_dir(n2o), dir="", post_write, img_tool, post_target, size=[{200, 200}]}).

% Actions
-record(action,  {?ACTION_BASE(undefined)}).
-record(wire,    {?ACTION_BASE(action_wire)}).
-record(api,     {?ACTION_BASE(action_api), name, tag, delegate }).
-record(event,   {?ACTION_BASE(action_event), type=default, postback, delegate}).
-record(control, {?ACTION_BASE(action_control), type=default, delegate}).
-record(alert,   {?ACTION_BASE(action_alert), text}).
-record(confirm, {?ACTION_BASE(action_confirm), text, postback, delegate}).
-record(jq,      {?ACTION_BASE(action_jq), property, method, args=[], right, format="~s"}).

% REST macros
-define(rest(), is_rest() -> true).
-define(unmap(Record), unmap(P,R) -> wf_utils:hunmap(P,R,record_info(fields, Record),size(R)-1)).
-define(map(Record), map(O) ->
    Y = [ try N=lists:nth(1,B), if is_number(N) -> wf:to_binary(B); true -> B end catch _:_ -> B end
          || B <- tl(tuple_to_list(O)) ],
    lists:zip(record_info(fields, Record), Y)).

-endif.
