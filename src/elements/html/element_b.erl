-module(element_b).
-author('doxtop@synrc.com').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, b).

render_element(Record = #b{}) ->
  wf_tags:emit_tag(<<"b">>, [Record#b.text], [
    {<<"id">>, Record#b.id},
    {<<"class">>, Record#b.class},
    {<<"style">>, Record#b.style}
  ]).

