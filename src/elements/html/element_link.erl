% vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_link).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, link).

render_element(Record) -> 
    ID = Record#link.id,
    Anchor = Record#link.anchor,
    case Record#link.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=click, postback=Postback, validation_group=ID, delegate=Record#link.delegate })
    end,

    Body = [
        wf:html_encode(Record#link.text, Record#link.html_encode),
        Record#link.body
    ],

    Target = target(Record#link.new),

    %% Basically, the default for mobile_target is to say nothing and let
    %% jquery mobile use its default setting. Anything other than a boolean
    %% will just treat it as blank

    DataFields1 = add_field(Record#link.mobile_target==false,{ajax,false},Record#link.data_fields),
    DataFields2 = add_field(Record#link.mobile_dialog==true,{rel,dialog},DataFields1),

    wf_tags:emit_tag(a, Body, [
        {id, Record#link.html_id},
        {href, wf:to_list(Record#link.url)},
        {class, [link, Record#link.class]},
        {target, Target},
        {style, Record#link.style},
        {title, wf:html_encode(Record#link.title, Record#link.html_encode)},
        {data_fields, DataFields2}
    ]).

target(New) ->
    case New of
        false -> "";
        true -> "_blank";
        _ -> ""
    end.

add_field(true,ToAdd,DataFields) -> [ToAdd | DataFields];
add_field(_,_,DataFields) -> DataFields.
