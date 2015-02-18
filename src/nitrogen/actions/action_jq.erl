-module(action_jq).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record=#jq{property=undefined,target=Target,method=Methods,args=Args0,format=_F}) ->
    Args = case Record#jq.format of "'~s'" -> [wf:render(Args0)]; _ -> Args0 end,
    RenderedArgs = string:join([ case A of 
        A when is_tuple(A) -> wf:render(A);
        A when is_list(A) -> A;
        A when is_integer(A) -> wf:to_list(A);
        A -> A end || A <- Args],","),
    string:join([ wf:f("qi('~s').~s("++Record#jq.format++");",
        [wf:to_list(Target),wf:to_list(Method),RenderedArgs]) || Method <- Methods],[]);

render_action(#jq{target=T,method=undefined,property=P,args=simple,right=R,format=_F}) ->
    wf:f("~s.~s = '~s';",
        [wf:to_list(T),wf:to_list(P),binary_to_list(iolist_to_binary(wf:render(R)))]);

render_action(#jq{target=T,method=undefined,property=P,right=undefined}) ->
    wf:f("qi('~s').~s;", [wf:to_list(T),wf:to_list(P)]);

render_action(#jq{target=T,method=undefined,property=P,right=R,format=_F}) ->
    wf:f("qi('~s').~s = '~s';",
        [wf:to_list(T),wf:to_list(P),binary_to_list(iolist_to_binary(wf:render(R)))]).
