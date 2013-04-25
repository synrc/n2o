% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_label).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, label).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#label.text, Record#label.html_encode),
        Record#label.body
    ],
    wf_tags:emit_tag(label, Body, [
        {id, Record#label.html_id},
        {class, [label, Record#label.class]},
        {style, Record#label.style},
        {for, Record#label.for}
    ]).
