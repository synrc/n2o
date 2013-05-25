-module(element_em).
-author('Sergei Lebedev').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, em).

render_element(Record) ->
  wf_tags:emit_tag(<<"em">>, wf:render(Record#em.body), [
    {<<"class">>, Record#em.class},
    {<<"style">>, Record#em.style}
  ]).
