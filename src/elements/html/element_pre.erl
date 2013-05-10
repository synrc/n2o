-module(element_pre).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, pre).

render_element(Record) ->
    Body = wf:html_encode(Record#pre.text, Record#pre.html_encode),
    wf_tags:emit_tag(pre, Body, [
        {class, [pre, Record#pre.class]},
        {style, Record#pre.style}
    ]).
