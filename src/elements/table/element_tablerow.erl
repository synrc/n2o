-module(element_tablerow).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tablerow).

render_element(Record) -> 
    Cells = Record#tablerow.cells,
    wf_tags:emit_tag(tr, Cells, [
        {id, Record#tablerow.id},
        {class, [tablerow, Record#tablerow.class]},
        {style, Record#tablerow.style}
    ]).
