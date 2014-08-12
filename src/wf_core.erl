-module (wf_core).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

transition(Actions) -> receive {init,A} -> transition(A); {'N2O',Pid} -> Pid ! Actions end.
run(Req) ->
    Pid = spawn(fun() -> transition([]) end),
    wf_context:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                                         "port:'", wf:to_list(wf:config(n2o,port,8000)),"'}"]),
    Ctx = wf_context:init_context(Req),
    Ctx1 = fold(init,Ctx#context.handlers,Ctx),
    wf_context:actions(Ctx1#context.actions),
    wf_context:context(Ctx1),
    Elements = try (Ctx1#context.module):main() catch C:E -> wf:error_page(C,E) end,
    Html = render(Elements),
    Actions = wf_context:actions(),
    Pid ! {init,Actions},
    Ctx2 = fold(finish,Ctx#context.handlers,Ctx1),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#context.req)),
    wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(200, Req2).

fold(Fun,Handlers,Ctx) ->
    lists:foldl(fun({_,Module},Ctx1) ->
        {ok,_,NewCtx} = Module:Fun([],Ctx1),
        NewCtx end,Ctx,Handlers).

render_item(E) when element(2,E) == element -> wf_render_elements:render_element(E);
render_item(E) when element(2,E) == action  -> wf_render_actions:render_action(E);
render_item(E) -> E.
render(<<E/binary>>) -> E;
render(undefined) -> [];
render(Elements) when is_list(Elements) -> [ render_item(E) || E <- lists:flatten(Elements) ];
render(Elements) -> render_item(Elements).

set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).
