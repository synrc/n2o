-module(element_strong).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, strong).

render_element(Record) ->
  wf_tags:emit_tag(<<"strong">>, wf:render(Record#strong.body), [
    {<<"class">>, Record#strong.class},
    {<<"style">>, Record#strong.style}
  ]).
