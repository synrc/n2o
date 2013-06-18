-module(element_figcaption).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record = #figcaption{}) -> 
  wf_tags:emit_tag(<<"figcaption">>, wf:render(Record#figcaption.body),
    lists:append([
      [{<<"id">>,   Record#figcaption.id},
      {<<"class">>, Record#figcaption.class},
      {<<"style">>, Record#figcaption.style}],
      Record#figcaption.data_fields,
      Record#figcaption.aria_states])).
