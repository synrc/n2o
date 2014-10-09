-module(n2o_websocket).
-description('WebSocket endpoint handler').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

protocols() -> wf:config(n2o,protocols,[ n2o_binary,
                                         n2o_rails,
                                         n2o_client,
                                         n2o_nitrogen,
                                         n2o_heart
                                          ]).

% web server callbacks

stream({text,Data}=Message, Req, State)   -> push(Message,Req,State,protocols(),[]);
stream({binary,Data}=Message, Req, State) -> push(binary_to_term(Data,[safe]),Req,State,protocols(),[]).

info({init_reply,Message}, Req, State)    -> reply(Message,Req,State);
info({init_n2o,P}, Req, State)            -> push({init,P},Req,State,protocols(),[]);
info(Message, Req, State)                 -> push(Message,Req,State,protocols(),[]).

terminate(_Req, _State=#cx{module=Module}) -> catch Module:event(terminate).
init(Req) ->
    wf:actions([]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_context:fold(init,Ctx#cx.handlers,Ctx),
    wf:context(NewCtx),
    wf:reg(broadcast,{wf:peer(Req)}),
    Req1 = wf:header(<<"Access-Control-Allow-Origin">>, <<"*">>, NewCtx#cx.req),
    {ok, Req1, NewCtx}.

% N2O top level protocol NOP REPLY PUSH

nop(R,S) ->       wf:info(?MODULE,"NOP",[]),        {reply,<<>>,R,S}.
reply(M,R,S) ->   wf:info(?MODULE,"REPLY ~p",[M]),  {reply,M,R,S}.

push(M,R,S,[],Acc)               -> nop(R,S);
push(M,R,S,[H|T]=Protocols,Acc)  -> wf:info(?MODULE,"PUSH ~p message ~p",[H,M]),
    case H:info(M,R,S) of
         {unknown,_,_,_}         -> push(M,R,S,T,Acc);
         {reply,Msg,Req,State}   -> reply(Msg,Req,State);
                    Ans          -> push(M,R,S,T,[Ans|Acc]) end.
