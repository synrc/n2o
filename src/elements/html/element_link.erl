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

    %% Basically, the default for mobile_target is to say nothing and let
    %% jquery mobile use its default setting. Anything other than a boolean
    %% will just treat it as blank

%    DataFields1 = add_field(Record#link.mobile_target==false,{<<"ajax">>,false},Record#link.data_fields),
%    DataFields2 = add_field(Record#link.mobile_dialog==true,{<<"rel">>,dialog},DataFields1),
    List = [{<<"id">>, Record#link.id},{<<"href">>, Record#link.url}],
    List1 = wf:append(List,<<"class">>, Record#link.class),
    List2 = wf:append(List1,<<"target">>, Target),
    List3 = wf:append(List2,<<"style">>, Record#link.style),
    List4 = wf:append(List3,<<"title">>, Record#link.title),
    wf_tags:emit_tag(<<"a">>, Body, List4).

target(New) ->
    case New of
        false -> "";
        true -> "_blank";
        _ -> ""
    end.

add_field(true,ToAdd,DataFields) -> [ToAdd | DataFields];
add_field(_,_,DataFields) -> DataFields.
