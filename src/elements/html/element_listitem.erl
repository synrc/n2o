-module(element_listitem).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, listitem).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#listitem.text, Record#listitem.html_encode),
        Record#listitem.body
    ],

    wf_tags:emit_tag(li, Body, [
        {id, Record#listitem.id},
        {class, [listitem, Record#listitem.class]},
        {role, Record#listitem.role},
        {style, Record#listitem.style},
        {data_fields, Record#listitem.data_fields}
    ]).
