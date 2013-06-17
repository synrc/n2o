-module(element_h1).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h1).

render_element(Record) -> 
  wf_tags:emit_tag(<<"h1">>, wf:render(Record#h1.body), [
    {<<"id">>,    Record#h1.id},
    {<<"class">>, Record#h1.class},
    {<<"style">>, Record#h1.style}
  ]).
