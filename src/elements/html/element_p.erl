% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_p).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, p).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#p.text, Record#p.html_encode),
        Record#p.body
    ],
    wf_tags:emit_tag(p, Body, [
        {id, Record#p.html_id},
        {class, [p, Record#p.class]},
        {style, Record#p.style}
    ]).

