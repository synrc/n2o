-module(element_hr).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, hr).

render_element(Record) -> 
  wf_tags:emit_tag(<<"hr">>, [
    {<<"id">>, Record#hr.id},
    {<<"class">>, Record#hr.class},
    {<<"style">>, Record#hr.style}
  ]).
