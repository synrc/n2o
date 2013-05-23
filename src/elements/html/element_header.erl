-module(element_header).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, header).

render_element(Record) ->
  wf_tags:emit_tag(<<"header">>, wf:render(Record#header.body), [
    {<<"id">>, Record#header.id},
    {<<"class">>, Record#header.class},
    {<<"style">>, Record#header.style}
  ]).
