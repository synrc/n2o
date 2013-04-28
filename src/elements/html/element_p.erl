-module (element_p).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

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

