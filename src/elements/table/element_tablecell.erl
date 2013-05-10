-module(element_tablecell).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tablecell).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#tablecell.text, Record#tablecell.html_encode),
        Record#tablecell.body
    ],

    wf_tags:emit_tag(td, Body, [
        {id, Record#tablecell.id},
        {class, [tablecell, Record#tablecell.class]},
        {style, Record#tablecell.style},
        {align, Record#tablecell.align},
        {valign, Record#tablecell.valign},
        {colspan, Record#tablecell.colspan},
        {rowspan, Record#tablecell.rowspan}	
    ]).
