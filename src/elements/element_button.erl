-module (element_button).
-author('Andrew Zadorozhny').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, button).

render_element(Record) ->
    Anchor = Record#button.anchor,

    Id = case Record#button.postback of
        undefined -> Record#button.id;
        Postback ->
          ID = case Record#button.id of undefined -> wf:temp_id(); I -> I end,
          wf:wire(Anchor, #event { type=click, postback=Postback, validation_group=ID, source=Record#button.source,  delegate=Record#button.delegate }), ID
    end,

  wf_tags:emit_tag(<<"button">>, wf:render(Record#button.body), [
      {<<"id">>, Id},
      {<<"type">>, Record#button.type},
      {<<"name">>, Record#button.name},
      {<<"class">>, Record#button.class},
      {<<"style">>, Record#button.style},
      {<<"value">>, Record#button.value}  | Record#button.data_fields
  ]).
