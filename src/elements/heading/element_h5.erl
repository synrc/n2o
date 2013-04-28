-module(element_h5).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h5).

render_element(Record) ->
    Text = wf:html_encode(Record#h5.text, Record#h5.html_encode),
    wf_tags:emit_tag(h5, Text, [
        {id, Record#h5.id},
        {class, [h5, Record#h5.class]},
        {style, Record#h5.style}
    ]).
