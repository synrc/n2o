-module(element_checkbox).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, checkbox).

render_element(Record) -> 
    Id = case Record#checkbox.id of undefined -> wf:temp_id(); I->I end,
    case Record#checkbox.postback of
        undefined -> ignore;
        Postback -> wf:wire(Id, #event { type=change, postback=Postback, validation_group=Id, source=Record#checkbox.source, delegate=Record#checkbox.delegate })
    end,
    Label = [ wf_tags:emit_tag(<<"input">>, [], [
                {<<"name">>, Record#checkbox.name},
                {<<"id">>,   Id},
                {<<"type">>, <<"checkbox">>},
                {<<"data-toggle">>, <<"checkbox">>},
                {<<"value">>, Record#checkbox.value},
                {<<"checked">>, if Record#checkbox.checked==true -> <<"checked">>; true -> undefined end} ]),
              case Record#checkbox.body of undefined -> []; B -> B end ],
    wf_tags:emit_tag(<<"label">>, wf:render(Label), [
        {<<"class">>, Record#checkbox.class},
        {<<"style">>, Record#checkbox.style},
        {<<"for">>, Id} ]).
