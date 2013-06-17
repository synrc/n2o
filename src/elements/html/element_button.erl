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

  wf_tags:emit_tag(<<"button">>, wf:render(Record#button.body), [
      {<<"id">>, ID},
      {<<"type">>, Record#button.type},
      {<<"name">>, Record#button.name},
      {<<"class">>, Record#button.class},
      {<<"style">>, Record#button.style},
      {<<"value">>, Record#button.value}  | Record#button.data_fields
  ]).
