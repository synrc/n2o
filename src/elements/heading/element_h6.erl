-module(element_h6).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h6).

render_element(Record) ->
  wf_tags:emit_tag(<<"h6">>, wf:render(Record#h6.body), [
    {<<"id">>, Record#h6.id},
    {<<"class">>, Record#h6.class},
    {<<"style">>, Record#h6.style}
  ]).
