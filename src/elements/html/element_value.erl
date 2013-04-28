-module(element_value).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, value).

render_element(Record) -> 
    Text = wf:html_encode(Record#value.text, Record#value.html_encode),
    wf_tags:emit_tag(span, Text, [
        {id, Record#value.id},
        {class, [value, Record#value.class]},
        {style, Record#value.style}
    ]).
