-module(element_li).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, li).

render_element(Record) -> 
  Body = [
    Record#li.text,
    wf:render(Record#li.body)
  ],

  wf_tags:emit_tag(<<"li">>, Body, [
    {<<"id">>, Record#li.id},
    {<<"class">>, [Record#li.class]},
    {<<"style">>, Record#li.style}
  ]).
