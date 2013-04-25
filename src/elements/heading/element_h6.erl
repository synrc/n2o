% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_h6).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h6).

render_element(Record) ->
    Text = wf:html_encode(Record#h6.text, Record#h6.html_encode),
    wf_tags:emit_tag(h6, Text, [
        {id, Record#h6.html_id},
        {class, [h6, Record#h6.class]},
        {style, Record#h6.style}
    ]).
