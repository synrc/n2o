-module(element_p).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, p).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#p.text, Record#p.html_encode),
        Record#p.body
    ],
    wf_tags:emit_tag(p, Body, [
        {id, Record#p.id},
        {class, [p, Record#p.class]},
        {style, Record#p.style}
    ]).

