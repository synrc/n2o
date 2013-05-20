-module(element_panel).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, panel).
render_element(Record) -> 
    Body = [
        Record#panel.text, %wf:html_encode(Record#panel.text, Record#panel.html_encode),
        Record#panel.body
    ], 
    wf_tags:emit_tag(<<"div">>, Body, [
        {<<"id">>, wf:to_binary(Record#panel.id)},
        {<<"class">>, wf:to_binary(Record#panel.class)},
        {<<"style">>, Record#panel.style},
        {<<"data_fields">>, Record#panel.data_fields}
    ]).
