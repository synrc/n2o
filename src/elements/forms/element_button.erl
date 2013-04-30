-module (element_button).
-author('Andrew Zadorozhny').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, button).

render_element(Record) ->
    ID = Record#button.id,
    Anchor = Record#button.anchor,
    case Record#button.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=click, 
                                             validation_group=ID, 
                                             postback=Postback, 
                                             source=Record#button.source,
                                             delegate=Record#button.delegate }) end,

    case Record#button.click of
         undefined -> ignore;
         ClickActions -> wf:wire(Anchor, #event { type=click, actions=ClickActions }) end,

    Value = ["  ", wf:html_encode(Record#button.text, Record#button.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {id, Record#button.id},
        {type, button},
        {class, [button, Record#button.class]},
        {style, Record#button.style},
        {value, Value}
    ]).
