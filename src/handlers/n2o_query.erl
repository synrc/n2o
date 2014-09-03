-module(n2o_query).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?QUERING_API).

init(State, Ctx) -> 
    {Params,NewReq} = wf:params(Ctx#context.req),
    {Form,NewReq2} = wf:form(NewReq),
    NewCtx = Ctx#context{params=Params,req=NewReq2,form=Form},
    {ok, [], NewCtx}.

finish(State, Ctx) ->  {ok, [], Ctx}.
