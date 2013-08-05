-module (action_comet).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

comet(Fun) -> comet("comet",Fun).
comet(Name, F) ->
    Pid = case global:whereis_name(Name) of
        undefined -> 
            Closure = fun(Fun) ->
                R = global:register_name(Name,self()),
                case R of
                    yes -> Fun();
                    _ -> skip end end,
            spawn(fun() -> wf_context:init_context([]), Closure(F) end);
        Registered -> Registered end,
    {ok,Pid}.

flush(Pool) ->
    Actions = wf_context:actions(),
    wf_context:clear_actions(),
    wf:send(Pool,{flush,Actions}).
