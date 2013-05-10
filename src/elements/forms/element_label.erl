-module(element_label).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, label).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#label.text, Record#label.html_encode),
        Record#label.body
    ],
    wf_tags:emit_tag(label, Body, [
        {id, Record#label.id},
        {class, [label, Record#label.class]},
        {style, Record#label.style},
        {for, Record#label.for}
    ]).
