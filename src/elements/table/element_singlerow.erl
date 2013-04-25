% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_singlerow).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, singlerow).

render_element(Record) -> 
    Table = #table {
        html_id=Record#singlerow.html_id,
        id=Record#singlerow.id,
        anchor=Record#singlerow.anchor,
        class=[singlerow, Record#singlerow.class],
        style=Record#singlerow.style,
        rows=#tablerow { cells=Record#singlerow.cells }
    },
    element_table:render_element(Table).
