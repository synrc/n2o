-module(wf_tags).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).
-define(NO_SHORT_TAGS(TagName),(TagName =/= 'div' andalso 
    TagName =/= 'span' andalso  TagName =/= 'label' andalso 
    TagName =/= 'textarea' andalso TagName =/= 'table' andalso 
    TagName =/= 'tr' andalso TagName =/= 'th' andalso 
    TagName =/= 'td' andalso TagName =/= 'p' andalso
    TagName =/= 'a' andalso TagName =/= 'ul' andalso
    TagName =/= 'ol' andalso TagName =/= 'select' andalso
    TagName =/= 'script' andalso TagName =/= 'iframe')).

emit_tag(TagName, Props) ->
    BinaryTag = list_to_binary(atom_to_list(TagName)),
    BinaryProps = write_props(Props),
    [<<"<">>,BinaryTag,BinaryProps,<<"/>">>].

emit_tag(TagName, [[], []], Props) when ?NO_SHORT_TAGS(TagName) -> emit_tag(TagName, Props);
emit_tag(TagName, [], Props) when ?NO_SHORT_TAGS(TagName) -> emit_tag(TagName, Props);
emit_tag(TagName, Content, Props) ->
    STagName = wf:to_list(TagName),
    [
        <<"<">>, 
        list_to_binary(STagName),
        write_props(Props), 
        <<">">>,
        Content,
        <<"</">>,
        list_to_binary(STagName),
        <<">">>
    ].

write_props(Props) ->  lists:map(fun display_property/1, Props).

display_property({Prop}) when is_atom(Prop) -> [<<" ">>, list_to_binary(atom_to_list(Prop)) ];
display_property({data_fields,DataTags}) -> [" ",data_tags(DataTags)];
display_property({id, Value}) -> [<<" id=\"">>, list_to_binary(wf:to_list(Value)) , <<"\"">>];
display_property({Prop, V}) when is_atom(Prop) -> display_property({atom_to_list(Prop), V});
display_property({Prop, []}) when Prop =/= "value" -> "";
display_property({Prop, Value}) when is_integer(Value); is_atom(Value); is_float(Value) -> [<<" ">>, list_to_binary(Prop), <<"=\"">>, list_to_binary(wf:to_list(Value)), <<"\"">>];
display_property({Prop, Value}) when is_binary(Value) -> [<<" ">>, list_to_binary(Prop), <<"=\"">>, Value, <<"\"">>];
display_property({Prop, Value}) when ?IS_STRING(Value) -> [<<" ">>, list_to_binary(Prop), <<"=\"">>, list_to_binary(Value), <<"\"">>];
display_property({Prop, Values}) ->
    StrValues = wf:to_string_list(Values),
    StrValues1 = string:strip(string:join(StrValues, " ")),
    StrValues2 = case Prop of
        "class" -> wf_utils:replace(StrValues1, ".", "");
        _ -> StrValues1
    end,
    [<<" ">>, list_to_binary(Prop), "=\"", list_to_binary(StrValues2), <<"\"">>].

data_tags(Data) -> [display_property({data_tag(FieldName),Value}) || {FieldName,Value} <- Data].
data_tag(FieldName) -> DataField = wf:to_binary(FieldName), "data-" ++ DataField.
