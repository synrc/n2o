-module(element_small).
-author('Andrew Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, small).

render_element(Record) ->
  wf_tags:emit_tag(<<"small">>, wf:render(Record#small.body), [
    {<<"class">>, Record#small.class},
    {<<"style">>, Record#small.style}
  ]).
