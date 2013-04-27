-module (element_h4).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h4).

render_element(Record) -> 
    Text = wf:html_encode(Record#h4.text, Record#h4.html_encode),
    wf_tags:emit_tag(h4, Text, [
        {id, Record#h4.id},
        {class, [h4, Record#h4.class]},
        {style, Record#h4.style}
    ]).
