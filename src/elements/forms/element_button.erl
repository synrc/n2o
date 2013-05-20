-module (element_button).
-author('Andrew Zadorozhny').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, button).

render_element(Record) ->
    ID = Record#button.id,
    Anchor = Record#button.anchor,
    case Record#button.postback of
         undefined -> skip;
         Postback -> wf:wire(Anchor, #event { type=click, 
                                              validation_group=ID,
                                              postback=Postback,
                                              source=Record#button.source }) end,

    case Record#button.click of
         undefined -> ignore;
         ClickActions -> wf:wire(Anchor, #event { type=click, actions=ClickActions }) end,

    List = [{<<"id">>, ID},{<<"type">>, <<"button">>}],
    List1 = wf:append(List,<<"style">>, Record#button.style),
    List2 = wf:append(List1,<<"value">>, Record#button.text),
    wf_tags:emit_tag(<<"input">>, List2).
