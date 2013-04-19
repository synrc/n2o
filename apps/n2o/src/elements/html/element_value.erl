% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_value).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, value).

render_element(Record) -> 
    Text = wf:html_encode(Record#value.text, Record#value.html_encode),
    wf_tags:emit_tag(span, Text, [
        {id, Record#value.html_id},
        {class, [value, Record#value.class]},
        {style, Record#value.style}
    ]).
