-module(n2o_websocket).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

% web server callbacks

protocols() -> wf:config(n2o,protocols,[ n2o_heart,n2o_nitrogen,n2o_client,n2o_binary ]).
stream({text,Data}=Message, Req, State) -> push(Message,Req,State,protocols(),[]);
stream({binary,Data}=Message, Req, State) -> push(Message,Req,State,protocols(),[]).
info(Message, Req, State) -> push(Message,Req,State,protocols(),[]).
terminate(_Req, _State=#context{module=Module}) -> catch Module:event(terminate).
init(Req) ->
    put(actions,[]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_core:fold(init,Ctx#context.handlers,Ctx),
    wf_context:context(NewCtx),
    wf:reg(broadcast,{wf:peer(Req)}),
    Req1 = wf:header(<<"Access-Control-Allow-Origin">>, <<"*">>, NewCtx#context.req),
    {ok, Req1, NewCtx}.

% N2O top level protocol

nop(R,S) -> {reply,<<>>,R,S}.
reply(M,R,S) -> {reply,M,R,S}.
push(Message, Req, State, [], Acc) -> nop(Req, State);
push(Message, Req, State, [H|T]=Protocols, Acc) ->
    wf:info(?MODULE,"call ~p message ~p",[H,Message]),
    case H:info(Message,Req,State) of
         {unknown,_,_,_} -> push(Message,Req,State,T,Acc);
         {reply,M,R,S}   -> reply(M,R,S);
              UnkAnswer  -> push(Message,Req,State,T,[UnkAnswer|Acc]) end.
