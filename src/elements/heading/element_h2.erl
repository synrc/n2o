% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h2).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h2).

render_element(Record) -> 
    Text = wf:html_encode(Record#h2.text, Record#h2.html_encode),
    wf_tags:emit_tag(h2, Text, [
        {id, Record#h2.html_id},
        {class, [h2, Record#h2.class]},
        {style, Record#h2.style}
    ]).
