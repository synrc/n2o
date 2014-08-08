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
       %% Checkbox...
        wf_tags:emit_tag(<<"input">>, [
            {<<"id">>, ID},
            {<<"value">>, Record#radio.value},

            %% the emitted name gives priority to html_name, but if it's
            %% undefined, then we fall back to the name attribute.
            %% Note, this might seem a bit hacky to have html_name and name
            %% that do essentially the same thing, but they have their own
            %% semantic meanings.  'html_name' is generally reserved for
            %% RESTful forms, while 'name' will be the more commonly used
            %% attribute.
            {<<"name">>, wf:coalesce([Record#radio.html_name,Record#radio.name])},
            {<<"type">>, <<"radio">>},
            {<<"class">>, Record#radio.class},
            {<<"style">>, Record#radio.style}
        ]),

        %% Label for Radio...
        wf_tags:emit_tag(<<"label">>, Content, [
            {<<"for">>, ID}
        ])
    ].
