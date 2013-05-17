-module (wf_core).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

run(Req) ->
    put(req,Req),
    call_init_on_handlers(),
    put(actions,[]),
    Module = get(page_module),
    Elements = Module:main(),
    Actions = wf_context:actions(),
    Pid = spawn(fun() -> transition(Actions) end),
    PidString = io_lib:format("~p",[Pid]),
    put(script,["TransitionProcess = '", PidString, "'"]),
    Html = wf_render_elements:render_elements(Elements),
    call_finish_on_handlers(),
    Req2 = cowboy_req:set_resp_body(Html, get(req)),
    {ok, ReqFinal} = cowboy_req:reply(200, Req2).

call_init_on_handlers() -> [wf_handler:handle(X, init) || X <- wf_context:handlers()].
call_finish_on_handlers() -> [wf_handler:handle(X, finish) || X <- wf_context:handlers()].

transition(Actions) -> receive {'N2O',Pid} -> Pid ! Actions end.
