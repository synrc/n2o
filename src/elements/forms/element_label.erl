-module(element_label).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, label).

render_element(Record) -> 
  wf_tags:emit_tag(<<"label">>, wf:render(Record#label.body), [
    {<<"id">>, Record#label.id},
    {<<"class">>, Record#label.class},
    {<<"style">>, Record#label.style},
    {<<"for">>, Record#label.for}
  ]).
