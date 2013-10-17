-module(action_jq).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record=#jq{property=undefined,target=Target,method=Methods}) ->
    Args = string:join([wf:f(Record#jq.format,[wf:to_binary(wf:render(A))]) 
        || A <- Record#jq.args],","),
    string:join([ wf:f("$('#~s').~s(~s);", [Target,Method,Args]) || Method <- Methods],[]);

render_action(#jq{target=Target,method=undefined,property=Property,args=simple,right=Right}) ->
    wf:info("simple"),
    wf:f("~s.~s = ~s;", [Target,Property,Right]);

render_action(#jq{target=Target,method=undefined,property=Property,right=undefined}) ->
    wf:f("$('#~s').~s;", [Target,Property]);

render_action(#jq{target=Target,method=undefined,property=Property,right=Right}) ->
    wf:f("$('#~s').~s = ~s", [Target,Property,Right]).
