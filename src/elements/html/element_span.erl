-module (element_span).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, span).

render_element(Record) -> 
  wf_tags:emit_tag(<<"span">>, wf:render(Record#span.body), [
    {<<"id">>, Record#span.id},
    {<<"class">>, Record#span.class},
    {<<"style">>, Record#span.style}
  ]).
