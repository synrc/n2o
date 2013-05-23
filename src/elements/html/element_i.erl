-module(element_i).
-author('doxtop@synrc.com').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, i).

render_element(Record = #i{}) ->
  wf_tags:emit_tag(<<"i">>, [Record#i.text], [
    {<<"id">>, Record#i.id},
    {<<"class">>, Record#i.class},
    {<<"style">>, Record#i.style}
  ]).

