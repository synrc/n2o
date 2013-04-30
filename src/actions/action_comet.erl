-module (action_comet).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

comet(Fun) ->
    Pid = spawn_link(fun() -> spawn_closure(Fun) end),
    {ok, Pid}.

spawn_closure(Fun) ->
    wf_context:init_context(undefined,undefined),
    Fun().

flush(Pool) ->
    Actions = wf_context:actions(),
    wf_context:clear_actions(),
    gproc:send({p,l,Pool},{flush, Actions}).
