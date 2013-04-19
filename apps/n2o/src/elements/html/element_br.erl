% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_br).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, br).

render_element(Record) -> 
    wf_tags:emit_tag(br, [
        {id, Record#br.html_id},
        {class, [br, Record#br.class]}, 
        {style, Record#br.style}
    ]).
