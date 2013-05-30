-module(element_tableheader).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tableheader).

render_element(Record) -> 
  wf_tags:emit_tag(<<"th">>, wf:render(Record#tableheader.body), [
    {<<"id">>, Record#tableheader.id},
    {<<"class">>, Record#tableheader.class},
    {<<"style">>, Record#tableheader.style}
  ]).
