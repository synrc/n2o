-module(element_tablerow).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tablerow).

render_element(Record) ->
  wf_tags:emit_tag(<<"tr">>, wf:render(Record#tablerow.cells), [
    {<<"id">>, Record#tablerow.id},
    {<<"class">>, Record#tablerow.class},
    {<<"style">>, Record#tablerow.style}
  ]).
