-module (element_h4).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h4).

render_element(Record) -> 
  wf_tags:emit_tag(<<"h4">>,  wf:render(Record#h4.body), [
    {<<"id">>, Record#h4.id},
    {<<"class">>, Record#h4.class},
    {<<"style">>, Record#h4.style}
  ]).
