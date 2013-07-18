-module(element_tr).
-include("wf.hrl").
-compile(export_all).

render_element(Record) ->
  wf_tags:emit_tag(<<"tr">>, wf:render(Record#tr.cells), [
    {<<"id">>, Record#tr.id},
    {<<"class">>, Record#tr.class},
    {<<"style">>, Record#tr.style}
  ]).
