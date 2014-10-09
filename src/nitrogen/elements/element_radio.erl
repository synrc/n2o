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

    Content = Record#radio.body,

    [
        wf_tags:emit_tag(<<"input">>, Content, [
            {<<"id">>, ID},
            {<<"value">>, Record#radio.value},
            {<<"name">>, wf:coalesce([Record#radio.html_name,Record#radio.name])},
            {<<"type">>, <<"radio">>},
            {<<"class">>, Record#radio.class},
            {<<"style">>, Record#radio.style}
        ])

    ].
