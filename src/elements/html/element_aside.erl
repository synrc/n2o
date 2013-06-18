-module(element_aside).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record) ->
  wf_tags:emit_tag(<<"aside">>, wf:render(Record#aside.body), [
    {<<"id">>,    Record#aside.id},
    {<<"class">>, Record#aside.class},
    {<<"style">>, Record#aside.style}
  ]).
