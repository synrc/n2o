-module(element_tableheader).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tableheader).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#tableheader.text, Record#tableheader.html_encode),
        Record#tableheader.body
    ],

    wf_tags:emit_tag(th, Body, [
        {id, Record#tableheader.id},
        {class, [tableheader, Record#tableheader.class]},
        {style, Record#tableheader.style},
        {align, Record#tableheader.align},
        {valign, Record#tableheader.valign},
        {colspan, Record#tableheader.colspan},
        {rowspan, Record#tableheader.rowspan}
    ]).
