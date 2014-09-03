-module(n2o_query).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?QUERING_API).

init(State, Ctx) -> 
    {Params,NewReq} = wf:params(Ctx#context.req),
    wf:info(?MODULE,"params = ~p ~p",[Params,NewReq]),
    NewCtx = Ctx#context{params=Params,req=NewReq},
    {ok, [], NewCtx}.

finish(State, Ctx) ->  {ok, [], Ctx}.
