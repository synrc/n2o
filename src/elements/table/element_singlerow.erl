-module(element_singlerow).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, singlerow).

render_element(Record) -> 
    Table = #table {
        id=Record#singlerow.id,
        class=[singlerow, Record#singlerow.class],
        style=Record#singlerow.style,
        rows=#tablerow { cells=Record#singlerow.cells }
    },
    element_table:render_element(Table).
