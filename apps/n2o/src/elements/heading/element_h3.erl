% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h3).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h3).

render_element(Record) -> 
    Text = wf:html_encode(Record#h3.text, Record#h3.html_encode),
    wf_tags:emit_tag(h3, Text, [
        {id, Record#h3.html_id},
        {class, [h3, Record#h3.class]},
        {style, Record#h3.style}
    ]).
