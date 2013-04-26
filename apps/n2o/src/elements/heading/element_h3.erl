-module(element_h3).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h3).

render_element(Record) -> 
    Text = wf:html_encode(Record#h3.text, Record#h3.html_encode),
    wf_tags:emit_tag(h3, Text, [
        {id, Record#h3.id},
        {class, [h3, Record#h3.class]},
        {style, Record#h3.style}
    ]).
