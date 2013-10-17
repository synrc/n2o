-module(n2o_query).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?QUERING_API).

init(State, Ctx) -> 
    Params = wf:params(Ctx#context.req),
    NewCtx = Ctx#context{params=Params},
    {ok, [], NewCtx}.

finish(State, Ctx) ->  {ok, [], Ctx}.
