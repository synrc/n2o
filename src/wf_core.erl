-module (wf_core).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

run(Req) ->
    Ctx = wf_context:init_context(Req),
    Ctx1 = fold(init,Ctx#context.handlers,Ctx),
    wf_context:actions([]),
    Module = Ctx1#context.module,
    wf_context:page_module(Module),
    Elements = Module:main(),
    Actions = wf_context:actions(),
    Pid = spawn(fun() -> transition(Actions) end),
    PidString = io_lib:format("~p",[Pid]),
    wf_context:script(["TransitionProcess = '", PidString, "'"]),
    Html = wf_render_elements:render_elements(Elements),
    Ctx2 = fold(finish,Ctx#context.handlers,Ctx1),
    Req2 = wf:response(Html,Ctx2#context.req),
    {ok, ReqFinal} = wf:reply(200, Req2).

fold(Fun,Handlers,Ctx) ->
    lists:foldl(fun(H,Ctx) ->
        {ok,_,NewCtx} = (H#handler.module):Fun(H#handler.state,Ctx),
        NewCtx end,Ctx,Handlers).

transition(Actions) -> receive {'N2O',Pid} -> Pid ! Actions end.
