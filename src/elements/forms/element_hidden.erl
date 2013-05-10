-module(element_hidden).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render_element(Record) -> 

    Value = wf:html_encode(Record#hidden.text, Record#hidden.html_encode),

    Disabled = case Record#hidden.disabled of
        true -> [{disabled}];
        false -> []
    end,

    wf_tags:emit_tag(input, Disabled ++ [
        {id, Record#hidden.id},
        {class, Record#hidden.class},
        {type, hidden},
        {name, Record#hidden.html_name},
        {value, Value}
    ]).
