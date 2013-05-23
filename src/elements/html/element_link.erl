-module(element_link).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, link).

render_element(Record) -> 
    error_logger:info_msg("#Link: ~p",[Record]),

    ID = Record#link.id,
    Anchor = Record#link.anchor,
    case Record#link.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=click, postback=Postback, validation_group=ID, delegate=Record#link.delegate })
    end,

    Body = [
        Record#link.text,
        wf:render(Record#link.body)
    ],

    Target = target(Record#link.new),

    List = [{<<"id">>, Record#link.id},
      {<<"href">>, Record#link.url},
      {<<"class">>, Record#link.class},
      {<<"target">>, Target},
      {<<"style">>, Record#link.style},
      {<<"title">>, Record#link.title},
      {<<"name">>, Record#link.name} | Record#link.data_fields
    ],
    wf_tags:emit_tag(<<"a">>, Body, List).

target(New) ->
    case New of
        false -> "";
        true -> "_blank";
        _ -> ""
    end.

add_field(true,ToAdd,DataFields) -> [ToAdd | DataFields];
add_field(_,_,DataFields) -> DataFields.
