-module(element_strong).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, strong).

render_element(Record) ->
    Body = [
        wf:html_encode(Record#strong.text, Record#strong.html_encode),
        Record#strong.body
    ],
    wf_tags:emit_tag(<<"strong">>, Body, [
        {<<"class">>, Record#strong.class},
        {<<"style">>, Record#strong.style}
    ]).
