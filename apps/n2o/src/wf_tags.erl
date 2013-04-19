% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Contributions from Tom McNulty (tom.mcnulty@cetiforge.com)
% See MIT-LICENSE for licensing information.

-module (wf_tags).
-author('tom.mcnulty@cetiforge.com').
-include_lib ("wf.hrl").
-define(NO_SHORT_TAGS(TagName),(
    TagName =/= 'div' andalso 
    TagName =/= 'span' andalso 
    TagName =/= 'label' andalso 
    TagName =/= 'textarea' andalso 
    TagName =/= 'table' andalso 
    TagName =/= 'tr' andalso 
    TagName =/= 'th' andalso 
    TagName =/= 'td' andalso 
    TagName =/= 'p' andalso
    TagName =/= 'a' andalso
    TagName =/= 'ul' andalso
    TagName =/= 'ol' andalso
	TagName =/= 'select' andalso
	TagName =/= 'script' andalso
    TagName =/= 'iframe')).

-export ([emit_tag/2, emit_tag/3, html_name/2]).

html_name(Id, Name)
  when Id =:= [] orelse
       Id =:= undefined ->
    html_name(wf_render_elements:temp_id(), Name);
html_name(Id, Name) ->
    case Name of
        undefined -> {name, Id};
        ""        -> {name, Id};
        Name      -> {name, Name}
    end.

%%%  Empty tags %%%

emit_tag(TagName, Props) ->
    STagName = wf:to_list(TagName),
    [
        "<",
        STagName,
        write_props(Props),
        "/>"
    ].

%%% Tags with child content %%%

% empty text and body
emit_tag(TagName, [[], []], Props) when ?NO_SHORT_TAGS(TagName) ->
    emit_tag(TagName, Props);

emit_tag(TagName, [], Props) when ?NO_SHORT_TAGS(TagName) ->
    emit_tag(TagName, Props);

emit_tag(TagName, Content, Props) ->
    STagName = wf:to_list(TagName),
    [
        "<", 
        STagName, 
        write_props(Props), 
        ">", 
        Content,
        "</", 
        STagName, 
        ">"
    ].    

%%% Property display functions %%%

write_props(Props) ->
    lists:map(fun display_property/1, Props).

display_property({Prop}) when is_atom(Prop) ->
    [" ", atom_to_list(Prop)];

%% Data fields are special in HTML5.
%% In this case, the DataTags value is expected to be a
%% proplist of [{field,Value}]. Emitted will be data-field="value".
%% "data-" gets prefixed on the fieldnames.
display_property({data_fields,DataTags}) ->
	[" ",data_tags(DataTags)];

display_property({Prop, V}) when is_atom(Prop) ->
    display_property({atom_to_list(Prop), V});

%% Most HTML tags don't care about a property with an empty string as its value
%% Except for the "value" tag on <option> and other form tags.
%% In this case, we emit the 'value' propery even if it's an empty value.
display_property({Prop, []}) when Prop =/= "value" -> "";    

display_property({Prop, Value}) when is_integer(Value); is_atom(Value); is_float(Value) ->
    [" ", Prop, "=\"", wf:to_list(Value), "\""];

display_property({Prop, Value}) when is_binary(Value); ?IS_STRING(Value) ->
    [" ", Prop, "=\"", Value, "\""];

display_property({Prop, Values}) ->
    StrValues = wf:to_string_list(Values),
    StrValues1 = string:strip(string:join(StrValues, " ")),
    StrValues2 = case Prop of
        "class" -> wf_utils:replace(StrValues1, ".", "");
        _ -> StrValues1
    end,
    [" ", Prop, "=\"", StrValues2, "\""].

%% 
data_tags(Data) ->
	[display_property({data_tag(FieldName),Value}) || {FieldName,Value} <- Data].

data_tag(FieldName) ->
	DataField = wf:to_binary(FieldName),
	<<"data-",DataField/binary>>.
