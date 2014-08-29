-module(element_legend).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, legend).

render_element(Record) -> 
  wf_tags:emit_tag(<<"legend">>, wf:render(Record#legend.body), [
    {<<"id">>, Record#legend.id},
    {<<"class">>, Record#legend.class},
    {<<"style">>, Record#legend.style}
  ]).
