-module(n2o_query).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?QUERING_API).

init(_State, Ctx) -> 
    {Params,NewReq} = wf:params(Ctx#cx.req),
    %{Form,NewReq2} = wf:form(NewReq),
    NewCtx = Ctx#cx{params=Params,req=NewReq},%,form=Form},
    {ok, [], NewCtx}.

finish(_State, Ctx) ->  {ok, [], Ctx}.
