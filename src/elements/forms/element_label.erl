-module(element_label).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, label).

render_element(Record) -> 
    Body = [
        Record#label.text,
        Record#label.body
    ],
    wf_tags:emit_tag(<<"label">>, Body, [
        {<<"id">>, Record#label.id},
        {<<"class">>, Record#label.class},
        {<<"style">>, Record#label.style},
        {<<"for">>, Record#label.for}
    ]).
