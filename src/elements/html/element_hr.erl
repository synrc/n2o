% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_hr).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, hr).

render_element(Record) -> 
    wf_tags:emit_tag(hr, [
        {id, Record#hr.html_id},
        {size, 1},
        {class, [hr, Record#hr.class]},
        {style, Record#hr.style}
    ]).
