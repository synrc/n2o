% vim: sw=4 ts=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_textbox).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textbox).

render_element(Record) -> 
    ID = Record#textbox.id,
    Anchor = Record#textbox.anchor,
    case Record#textbox.next of
        undefined -> ignore;
        Next -> 
            Next1 = wf_render_actions:normalize_path(Next),
            wf:wire(Anchor, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next1]) })
    end,

    case Record#textbox.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=enterkey, postback=Postback, validation_group=ID, delegate=Record#textbox.delegate })
    end,

    Value = wf:html_encode(Record#textbox.text, Record#textbox.html_encode),
    Placeholder  = wf:html_encode(Record#textbox.placeholder, true),
    wf_tags:emit_tag(input, [
        {id, Record#textbox.html_id},
        {type, text}, 
        {class, [textbox, Record#textbox.class]},
        {maxlength, Record#textbox.maxlength},
        {style, Record#textbox.style},
        {name, Record#textbox.html_name},
        {placeholder, Placeholder},
        {value, Value}
    ]).
