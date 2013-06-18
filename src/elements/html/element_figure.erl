-module(element_figure).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record = #figure{}) -> 
  wf_tags:emit_tag(<<"figure">>, wf:render(Record#figure.body),
    lists:append([
      [{<<"id">>,   Record#figure.id},
      {<<"class">>, Record#figure.class},
      {<<"style">>, Record#figure.style}],
      Record#figure.data_fields,
      Record#figure.aria_states])).
