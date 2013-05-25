-module(element_value).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, value).

render_element(Record) -> 
  wf_tags:emit_tag(<<"span">>, wf:render(Record#value.body), [
    {<<"id">>, Record#value.id},
    {<<"class">>, Record#value.class},
    {<<"style">>, Record#value.style}
  ]).
