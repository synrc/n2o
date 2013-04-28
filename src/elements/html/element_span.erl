-module (element_span).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, span).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#span.text, Record#span.html_encode),
        Record#span.body
    ],

    wf_tags:emit_tag(span, Body, [
        {id, Record#span.id},
        {class, Record#span.class}, 
        {style, Record#span.style}
    ]).
