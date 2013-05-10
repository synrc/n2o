-module(element_textbox).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textbox).

render_element(Record) -> 
    ID = Record#textbox.id,
    Value = wf:html_encode(Record#textbox.text, Record#textbox.html_encode),
    Placeholder  = wf:html_encode(Record#textbox.placeholder, true),
    wf_tags:emit_tag(input, [
        {id, Record#textbox.id},
        {type, text}, 
        {class, [textbox, Record#textbox.class]},
        {maxlength, Record#textbox.maxlength},
        {style, Record#textbox.style},
        {name, Record#textbox.html_name},
        {placeholder, Placeholder},
        {value, Value}
    ]).
