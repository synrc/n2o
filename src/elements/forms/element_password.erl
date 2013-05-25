-module(element_password).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, password).

render_element(Record) -> 
    ID = Record#password.id,
    Anchor = Record#password.anchor,
    case Record#password.next of
        undefined -> ignore;
        Next -> wf:wire(Anchor, #event { type=enterkey, actions=wf:f("$('#~s').select();", [Next]) })
    end,
    case Record#password.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=enterkey, 
                                             postback=Postback, 
                                             validation_group=ID,
                                             delegate=Record#password.delegate })
    end,

    wf_tags:emit_tag(<<"input">>, [
        {<<"id">>, Record#password.id},
        {<<"type">>, <<"password">>},
        {<<"class">>, Record#password.class},
        {<<"style">>, Record#password.style},
        {<<"name">>, Record#password.html_name},
        {<<"value">>, Record#password.value}
    ]).
