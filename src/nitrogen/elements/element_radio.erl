-module(element_radio).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, radio).

render_element(Record) ->
    ID = case Record#radio.id of
        undefined -> wf:temp_id();
        RadioID -> RadioID
    end,

    case Record#radio.postback of
        undefined -> ignore;
        Postback -> wf:wire(#event{type=change, postback=Postback, target=ID, delegate=Record#radio.delegate })
    end,

    Content = wf:render(Record#radio.body),
    TypeChecked = case Record#radio.checked of
         true -> [{<<"checked">>, <<"">>},{<<"type">>, <<"radio">>}];
            _ -> [{<<"type">>, <<"radio">>}] end ++ case Record#radio.disabled of
         true -> [{<<"disabled">>, <<"disabled">>}];
            _ -> [] end,

    [
        wf_tags:emit_tag(<<"input">>, Content, TypeChecked ++ [
            {<<"id">>, ID},
            {<<"value">>, Record#radio.value},
            {<<"name">>, wf:coalesce([Record#radio.html_name,Record#radio.name])},
            {<<"class">>, Record#radio.class},
            {<<"style">>, Record#radio.style}
        ])

    ].
