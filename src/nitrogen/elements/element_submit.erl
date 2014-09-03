-module (element_submit).
-author('Andrew Zadorozhny').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, submit).

render_element(Record) ->
    ID = case Record#submit.id of undefined -> wf:temp_id(); I->I end,
    case Record#submit.postback of
         undefined -> skip;
         Postback -> wf:wire(#event { type=click, 
                                      target=ID,
                                      postback=Postback,
                                      source=Record#submit.source }) end,
    case Record#submit.click of
         undefined -> ignore;
         ClickActions -> wf:wire(#event { target=ID, type=click, actions=ClickActions }) end,
  wf_tags:emit_tag(<<"input">>, [
      {<<"id">>, ID},
      {<<"type">>, <<"submit">>},
      {<<"class">>, Record#submit.class},
      {<<"style">>, Record#submit.style},
      {<<"value">>, Record#submit.body}  | Record#submit.data_fields
  ]).
