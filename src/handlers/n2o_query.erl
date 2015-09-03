-module(n2o_query).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?QUERING_API).

init(_State, Ctx) ->
    {Params,NewReq} = wf:params(Ctx#cx.req),
    NewCtx = Ctx#cx{params=Params,req=NewReq},
    {ok, [], NewCtx}.

finish(_State, Ctx) ->  {ok, [], Ctx}.
