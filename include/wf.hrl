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
        data_fields=[], aria_states=[], body, role, tabindex, show_if=true, html_tag=Tag, title).
-define(ACTION_BASE(Module),
        ancestor=action, trigger, target, module=Module, actions, source=[]).
-define(CTRL_BASE(Module), ?ELEMENT_BASE(Module,undefined,Module)).

-record(element, {?ELEMENT_BASE(undefined)}).
-record(range, {?ELEMENT_BASE(element_range), min=0, max=100, step=1, value=0, next, postback}).
-record(literal, {?ELEMENT_BASE(element_literal)}).
-record(dtl, {?ELEMENT_BASE(element_dtl), file="index", bindings=[], app=web, folder="priv/templates", ext="html", bind_script=true }).
-record(list, {?ELEMENT_BASE(element_list), numbered=false }).
-record(label, {?ELEMENT_BASE(element_label), for=""}).
-record(link, {?ELEMENT_BASE(element_link), target, url="javascript:void(0);", postback, name, download}).
-record(submit, {?ELEMENT_BASE(element_submit), click, postback}).
-record(button, {?ELEMENT_BASE(element_button), type= <<"button">>, name, value, postback, disabled}).
-record(textbox, {?ELEMENT_BASE(element_textbox), value, disabled, maxlength="", placeholder="", next, postback, name, autofocus}).
-record(hidden, {?ELEMENT_BASE(element_hidden), value, html_name, disabled=false}).
-record(textarea, {?ELEMENT_BASE(element_textarea), placeholder, name, cols, rows, value}).
-record(optgroup, {?ELEMENT_BASE(element_select), label, disabled}).
-record(option, {?ELEMENT_BASE(element_select), label, value, selected=false, disabled}).
-record(dropdown, {?ELEMENT_BASE(element_dropdown), options, postback, value, multiple=false, disabled=false, name}).
-record(select, {?ELEMENT_BASE(element_select), disabled, multiple, name, size, postback}).
-record(checkbox, {?ELEMENT_BASE(element_checkbox), checked=false, value="on", postback, name}).
-record(radiogroup, {?ELEMENT_BASE(element_radiogroup)}).
-record(radio, {?ELEMENT_BASE(element_radio), value, name, checked=false, postback, html_name}).
-record(password, {?ELEMENT_BASE(element_password), value, maxlength="", placeholder= <<"password">>, next, postback, name}).
-record(spinner, {?ELEMENT_BASE(element_spinner), image="/nitrogen/spinner.gif"}).
-record(image, {?ELEMENT_BASE(element_image), image="", alt="", width, height}).
-record(table, {?ELEMENT_BASE(element_table), header, footer}).
-record(td, {?ELEMENT_BASE(element_td), colspan=1, rowspan=1, scope}).
-record(th, {?ELEMENT_BASE(element_th), colspan=1, rowspan=1, scope}).
-record(tr, {?ELEMENT_BASE(element_tr), cells, postback}).
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
-record(thead,      ?DEFAULT_BASE).

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
