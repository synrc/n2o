-module(element_h2).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h2).

render_element(Record) -> 
    Text = wf:html_encode(Record#h2.text, Record#h2.html_encode),
    wf_tags:emit_tag(h2, Text, [
        {id, Record#h2.id},
        {class, [h2, Record#h2.class]},
        {style, Record#h2.style}
    ]).
