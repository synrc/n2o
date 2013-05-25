-module(element_p).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, p).

render_element(Record) -> 
  wf_tags:emit_tag(<<"p">>, wf:render(Record#p.body), [
    {<<"id">>, Record#p.id},
    {<<"class">>, Record#p.class},
    {<<"style">>, Record#p.style}
  ]).

