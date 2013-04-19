% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hidden).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render_element(Record) -> 

    Value = wf:html_encode(Record#hidden.text, Record#hidden.html_encode),

    Disabled = case Record#hidden.disabled of
        true -> [{disabled}];
        false -> []
    end,

    wf_tags:emit_tag(input, Disabled ++ [
        {id, Record#hidden.html_id},
        {class, Record#hidden.class},
        {type, hidden},
        {name, Record#hidden.html_name},
        {value, Value}
    ]).
