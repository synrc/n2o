-module(n2o_document).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

transition(Actions) -> receive {'INIT',A} -> transition(A); {'N2O',Pid} -> Pid ! {actions,Actions} end.
run(Req) ->
    Pid = spawn(fun() -> transition([]) end),
    wf:script(["var transition = {pid: '", wf:pickle(Pid), "', ",
                "port:'", wf:to_list(wf:config(n2o,port)),"'}"]),
    Ctx = wf_context:init_context(Req),
    Ctx1 = wf_core:fold(init,Ctx#cx.handlers,Ctx),
    wf:actions(Ctx1#cx.actions),
    wf:context(Ctx1),
    Elements = try (Ctx1#cx.module):main() catch C:E -> wf:error_page(C,E) end,
    Html = wf_render:render(Elements),
    Actions = wf:actions(),
    Pid ! {'INIT',Actions},
    Ctx2 = wf_core:fold(finish,Ctx#cx.handlers,Ctx1),
    Req2 = wf:response(Html,wf_core:set_cookies(wf:cookies(),Ctx2#cx.req)),
    wf:info(?MODULE,"Cookies Req: ~p",[Req2]),
    {ok, _ReqFinal} = wf:reply(200, Req2).
