% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_radio).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, radio).

render_element(Record) -> 
    ID = Record#radio.id,
    Anchor = case Record#radio.anchor of
        "." ++ AnchorNoDot -> AnchorNoDot;
        A -> A
    end,
    CheckedOrNot = case Record#radio.checked of
        true -> checked;
        _ -> not_checked
    end,

    case Record#radio.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=change, postback=Postback, validation_group=ID, delegate=Record#radio.delegate })
    end,

    Content = wf:html_encode(Record#radio.text, Record#radio.html_encode),

    [
        %% Checkbox...
        wf_tags:emit_tag(input, [
            {id, Anchor},
            {value, Record#radio.value},

            %% the emitted name gives priority to html_name, but if it's
            %% undefined, then we fall back to the name attribute.
            %% Note, this might seem a bit hacky to have html_name and name
            %% that do essentially the same thing, but they have their own
            %% semantic meanings.  'html_name' is generally reserved for
            %% RESTful forms, while 'name' will be the more commonly used
            %% attribute.
            {name, wf:coalesce([Record#radio.html_name,Record#radio.name])},
            {type, radio},
            {class, [radio, Record#radio.class]},
            {style, Record#radio.style},
            {CheckedOrNot, true}
        ]),

        %% Label for Radio...
        wf_tags:emit_tag(label, Content, [
            {for, Anchor}
        ])
    ].
