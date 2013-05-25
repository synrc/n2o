-module(element_pre).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, pre).

render_element(Record) ->
  wf_tags:emit_tag(<<"pre">>, wf:render(Record#pre.body), [
    {<<"class">>, [pre, Record#pre.class]},
    {<<"style">>, Record#pre.style}
  ]).
