-module(element_td).
-include("wf.hrl").
-compile(export_all).

render_element(Record) ->
  wf_tags:emit_tag(<<"td">>, wf:render(Record#td.body), [
    {<<"id">>, Record#td.id},
    {<<"class">>, Record#td.class},
    {<<"style">>, Record#td.style},
    {<<"rowspan">>, Record#td.rowspan},
    {<<"colspan">>, Record#td.colspan},
    {<<"scope">>, Record#td.scope}
  ]).
