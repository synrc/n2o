% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablerow).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tablerow).

render_element(Record) -> 
    Cells = Record#tablerow.cells,
    wf_tags:emit_tag(tr, Cells, [
        {id, Record#tablerow.html_id},
        {class, [tablerow, Record#tablerow.class]},
        {style, Record#tablerow.style}
    ]).
