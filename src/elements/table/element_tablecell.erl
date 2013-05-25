-module(element_tablecell).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tablecell).

render_element(Record) -> 
  wf_tags:emit_tag(<<"td">>, wf:render(Record#tablecell.body), [
    {<<"id">>, Record#tablecell.id},
    {<<"class">>, Record#tablecell.class},
    {<<"style">>, Record#tablecell.style},
    {<<"align">>, Record#tablecell.align},
    {<<"valign">>, Record#tablecell.valign},
    {<<"colspan">>, Record#tablecell.colspan},
    {<<"rowspan">>, Record#tablecell.rowspan}	
  ]).
