-module(action_async).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

async(Fun) -> async("comet",Fun).
async(Name, F) ->
    Pid = case global:whereis_name(Name) of
        undefined ->
            Closure = fun(Fun) ->
                R = global:register_name(Name,self()),
                case R of
                    yes -> Fun();
                    _ -> skip end end,
            Req = case wf:context() of
                undefined -> undefined;
                _ -> ?REQ
            end,
            spawn(fun() -> init_context(Req), Closure(F) end);
        Registered -> Registered end,
    {ok,Pid}.

flush(Pool) ->
    Actions = wf_context:actions(),
    wf:actions([]),
    wf:send(Pool,{flush,Actions}).

init_context(undefined) -> [];
init_context(Req) ->
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_context:fold(init, Ctx#cx.handlers, Ctx),
    wf:actions(NewCtx#cx.actions),
    wf:context(NewCtx).
