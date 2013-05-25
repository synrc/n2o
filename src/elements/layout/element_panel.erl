-module(element_panel).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, panel).
render_element(Record) -> 
  wf_tags:emit_tag(<<"div">>, wf:render(Record#panel.body), [
    {<<"id">>, wf:to_binary(Record#panel.id)},
    {<<"class">>, Record#panel.class},
    {<<"style">>, Record#panel.style} | Record#panel.data_fields
  ]).
