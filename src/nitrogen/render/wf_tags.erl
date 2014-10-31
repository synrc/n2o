-module(wf_tags).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).
-define(VOID(Tag),  (Tag == <<"br">>     orelse Tag == <<"hr">>
              orelse Tag == <<"link">>   orelse Tag == <<"img">> 
              orelse Tag == <<"input">>  orelse Tag == <<"link">>
              orelse Tag == <<"meta">>   orelse Tag == <<"param">>
              orelse Tag == <<"base">>   orelse Tag == <<"area">>
              orelse Tag == <<"col">>    orelse Tag == <<"command">> orelse Tag == <<"option">>
              orelse Tag == <<"keygen">> orelse Tag == <<"source">>)).

emit_tag(TagName, Props) -> [<<"<">>,TagName] ++ write_props(Props) ++ [<<"/>">>].
emit_tag(TagName, undefined, Props) -> emit_tag(TagName, [], Props);
emit_tag(TagName, [undefined], Props) -> emit_tag(TagName, [], Props);
emit_tag(TagName, [], Props) when ?VOID(TagName) -> emit_tag(TagName, Props);
emit_tag(TagName, [], Props) -> [<<"<">>,TagName,write_props(Props),<<">">>,<<"</">>,TagName,<<">">>];
emit_tag(TagName, Content, Props) -> [<<"<">>,TagName,write_props(Props),<<">">>, Content,<<"</">>,TagName,<<">">>].
write_props(Props) -> lists:map(fun display_property/1, Props).
display_property({_, undefined}) -> [];
display_property({_, []}) -> [];
display_property({<<"class">>=Id, Value}) -> prop({Id,Value});
display_property({<<"data-toggle">>=Id, Value}) -> prop({Id,Value});
display_property({Prop, Value}) -> [<<" ">>, wf:to_binary(Prop), <<"=\"">>, wf:to_binary(Value), <<"\"">>].

prop({Id, Value}) when is_atom(Value) -> [<<" ">>,Id,<<"=\"">>, wf:to_binary(Value), <<"\"">>];
prop({Id, Value}) when is_binary(Value) -> [<<" ">>,Id,<<"=\"">>, Value, <<"\"">>];
prop({Id, Value}) -> [<<" ">>,Id,<<"=\"">>, string:join([wf:to_list(V) || V <-Value ]," "), <<"\"">>].
