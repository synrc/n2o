-module(action_jq).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record=#jq{property=undefined,target=Target,method=Methods,args=A,format=F}) ->
    Args = case Record#jq.format of "'~s'" -> [wf:render(A)]; _ -> A end,
    RenderedArgs = string:join([ case A of 
        A when is_tuple(A) -> wf:render(A);
        A when is_list(A) -> A;
        A when is_integer(A) -> wf:to_list(A);
        A -> A end || A <- Args],","),
    string:join([ wf:f("$('#~s').~s("++Record#jq.format++");",
        [Target,Method,RenderedArgs]) || Method <- Methods],[]);

render_action(#jq{target=Target,method=undefined,property=Property,args=simple,right=Right}) ->
    wf:f("~s.~s = ~s;", [Target,Property,Right]);

render_action(#jq{target=Target,method=undefined,property=Property,right=undefined}) ->
    wf:f("$('#~s').~s;", [Target,Property]);

render_action(#jq{target=Target,method=undefined,property=Property,right=Right}) ->
    wf:f("$('#~s').~s = ~s", [Target,Property,Right]).
