-module(element_textarea).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
  wf_tags:emit_tag(<<"textarea">>, wf:render(Record#textarea.body), [
    {<<"class">>, Record#textarea.class},
    {<<"id">>, Record#textarea.id},
    {<<"style">>, Record#textarea.style},
    {<<"name">>, Record#textarea.html_name},
    {<<"placeholder">>, Record#textarea.placeholder}
  ]).

