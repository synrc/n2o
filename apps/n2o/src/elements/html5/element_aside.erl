% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_aside).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, aside).

render_element(Record) ->
    wf_tags:emit_tag(aside, Record#aside.body, [
        {id, Record#aside.html_id},
        {class, ["aside", Record#aside.class]},
        {style, Record#aside.style}
    ]).
