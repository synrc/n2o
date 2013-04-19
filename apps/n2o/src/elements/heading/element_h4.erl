% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_h4).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h4).

render_element(Record) -> 
    Text = wf:html_encode(Record#h4.text, Record#h4.html_encode),
    wf_tags:emit_tag(h4, Text, [
        {id, Record#h4.html_id},
        {class, [h4, Record#h4.class]},
        {style, Record#h4.style}
    ]).
