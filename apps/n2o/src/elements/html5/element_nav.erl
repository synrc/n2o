% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_nav).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, nav).

render_element(Record) ->
    wf_tags:emit_tag(nav, Record#nav.body, [
        {id, Record#nav.html_id},
        {class, ["nav", Record#nav.class]},
        {style, Record#nav.style}
    ]).
