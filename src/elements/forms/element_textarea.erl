-module(element_textarea).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) -> 
    Text = Record#textarea.text,
    Placeholder  = Record#textarea.placeholder,
    wf_tags:emit_tag(<<"textarea">>, Text, [
        {<<"class">>, Record#textarea.class},
        {<<"id">>, Record#textarea.id},
        {<<"style">>, Record#textarea.style},
        {<<"name">>, Record#textarea.html_name},
        {<<"placeholder">>, Placeholder}
    ]).

