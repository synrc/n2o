% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_section).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, section).

render_element(Record) ->
    wf_tags:emit_tag(section, Record#section.body, [
        {id, Record#section.html_id},
        {class, ["section", Record#section.class]},
        {style, Record#section.style}
    ]).
