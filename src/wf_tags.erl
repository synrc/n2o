-module(wf_tags).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

-define(NO_SHORT_TAGS(TagName),(TagName =/= <<"div">> andalso 
    TagName =/= <<"span">> andalso  TagName =/= <<"label">> andalso 
    TagName =/= <<"textarea">> andalso TagName =/= <<"table">> andalso 
    TagName =/= <<"tr">> andalso TagName =/= <<"th">> andalso 
    TagName =/= <<"td">> andalso TagName =/= <<"p">> andalso
    TagName =/= <<"a">> andalso TagName =/= <<"ul">> andalso
    TagName =/= <<"ol">> andalso TagName =/= <<"select">> andalso
    TagName =/= <<"script">> andalso TagName =/= <<"iframe">>)).

emit_tag(TagName, Props) -> [<<"<">>,TagName] ++ write_props(Props) ++ [<<"/>">>].
emit_tag(TagName, [], Props) when ?NO_SHORT_TAGS(TagName) -> emit_tag(TagName, Props);
emit_tag(TagName, Content, Props) -> [<<"<">>,TagName,write_props(Props),<<">">>,Content,<<"</">>,TagName,<<">">>].
write_props(Props) -> lists:map(fun display_property/1, Props).
display_property({Prop, Value}) -> [<<" ">>, wf:to_binary(Prop), <<"=\"">>, wf:to_binary(Value), <<"\"">>].
