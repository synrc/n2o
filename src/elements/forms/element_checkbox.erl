-module(element_checkbox).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, checkbox).

render_element(Record) -> 
    Id = Record#checkbox.id,
    Anchor = case Record#checkbox.anchor of
        "." ++ AnchorNoDot -> AnchorNoDot;
        A -> A
    end,
    CheckedOrNot = case Record#checkbox.checked of
        true -> <<"checked">>;
        _ -> <<"not_checked">>
    end,
    case Record#checkbox.postback of
        undefined -> ignore;
        Postback -> wf:wire(Id, #event { type=change, postback=Postback, validation_group=Id, delegate=Record#checkbox.delegate })
    end,

    Text = Record#checkbox.text,
    Icons = [
      wf_tags:emit_tag(<<"span">>, [], [
        {<<"class">>, <<"first-icon fui-checkbox-unchecked">>}
      ]),
      wf_tags:emit_tag(<<"span">>, [], [
        {<<"class">>, <<"second-icon fui-checkbox-checked">>}
      ])
    ],
    Label = [
      wf_tags:emit_tag(<<"span">>, Icons, [{<<"class">>, <<"icons">>}]),
      wf_tags:emit_tag(<<"input">>, [
            {<<"name">>, Record#checkbox.html_name},
            {<<"id">>,   ID},
            {<<"type">>, <<"checkbox">>},
            {<<"data-toggle">>, <<"checkbox">>},
            {<<"class">>, Record#checkbox.class},
            {<<"style">>, Record#checkbox.style},
            {<<"value">>, Record#checkbox.value},
            {CheckedOrNot, true}
      ]),
      Text
    ],
    wf_tags:emit_tag(<<"label">>, Label, [
      {<<"class">>, Record#checkbox.class},
      {<<"style">>, Record#checkbox.style},
      {<<"for">>, Id}
    ]).
