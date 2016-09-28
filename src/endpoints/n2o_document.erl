-module(n2o_document).
-description('N2O Server Pages HTTP endpoint handler').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

transition(Actions) ->
    receive
        {'INIT',A} -> transition(A);
        {'N2O',Pid} -> Pid ! {actions,Actions}
    after wf:config(n2o,transition_timeout,30000) -> {timeout,transition}
    end.

run(Req) ->
    wf:state(status,200),
    Pid = spawn(fun() -> transition([]) end),
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,websocket_port,wf:config(n2o,port,8000))),"'}"]),
    Ctx = wf:init_context(Req),
    Ctx1 = wf:fold(init,Ctx#cx.handlers,Ctx),
    wf:actions(Ctx1#cx.actions),
    wf:context(Ctx1),
    Elements = try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end,
    Html = wf_render:render(Elements),
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = wf:fold(finish,Ctx#cx.handlers,?CTX),
    Req2 = wf:response(Html,set_cookies(wf:cookies(),Ctx2#cx.req)),
    wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(wf:state(status), Req2).

set_cookies([],Req)-> Req;
set_cookies([{Name,Value,Path,TTL}|Cookies],Req)->
    set_cookies(Cookies,wf:cookie_req(Name,Value,Path,TTL,Req)).

