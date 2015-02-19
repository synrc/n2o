-module(n2o_websocket).
-description('WebSocket endpoint handler').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

protocols() -> wf:config(n2o,protocols,[ n2o_binary,
                                         n2o_client,
                                         n2o_nitrogen,
                                         n2o_heart
                                       ]).

stream(<<>>, Req, State)                  -> nop(Req,State);
stream({text,_Data}=Message, Req, State)   -> push(Message,Req,State,protocols(),[]);
stream({binary,Data}=_Message, Req, State) -> push(binary_to_term(Data,[safe]),Req,State,protocols(),[]).
info(Message, Req, State)                 -> push(Message,Req,State,protocols(),[]).

terminate(_Req, _State=#cx{module=Module}) -> catch Module:event(terminate).
init(Req) ->
    wf:actions([]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_context:fold(init,Ctx#cx.handlers,Ctx),
    wf:context(NewCtx),
    wf:reg(broadcast,{wf:peer(Req)}),
    {Origin, _} = cowboy_req:header(<<"origin">>, Req, <<"*">>),
    ConfigOrigin = wf:to_binary(wf:config(n2o,origin,Origin)),
    wf:info(?MODULE,"Origin: ~p",[ConfigOrigin]),
    Req1 = wf:header(<<"Access-Control-Allow-Origin">>, ConfigOrigin, NewCtx#cx.req),
    {ok, Req1, NewCtx}.

% N2O top level protocol NOP REPLY PUSH

nop(R,S) ->       wf:info(?MODULE,"NOP",[]),        {reply,<<>>,R,S}.
reply(M,R,S) ->   wf:info(?MODULE,"REPLY~n~p",[M]),  {reply,M,R,S}.

push(_M,R,S,[],_Acc)               -> nop(R,S);
push(M,R,S,[H|T]=_Protocols,Acc)  -> wf:info(?MODULE,"PUSH ~p message~n~p",[H,M]),
    case H:info(M,R,S) of
         {unknown,_,_,_}         -> push(M,R,S,T,Acc);
         {reply,Msg,Req,State}   -> reply(Msg,Req,State);
                    Ans          -> push(M,R,S,T,[Ans|Acc]) end.
