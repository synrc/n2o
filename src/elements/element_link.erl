-module(element_link).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, link).

render_element(Record) -> 
    Anchor = Record#link.anchor,
    Id = case Record#link.postback of
        undefined -> Record#link.id;
        Postback ->
          ID = case Record#link.id of undefined -> wf:temp_id(); I -> I end,
          wf:wire(Anchor, #event { type=click, postback=Postback, validation_group=ID, source=Record#link.source,  delegate=Record#link.delegate }), ID
    end,

    List = [{<<"id">>, Id},
      {<<"href">>, Record#link.url},
      {<<"class">>, Record#link.class},
      {<<"target">>, Record#link.target},
      {<<"style">>, Record#link.style},
      {<<"title">>, Record#link.title},
      {<<"tabindex">>, Record#link.tabindex},
      {<<"name">>, Record#link.name} | Record#link.data_fields
    ],

    wf_tags:emit_tag(<<"a">>, wf:render(Record#link.body), List).
