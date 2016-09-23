-module(n2o_proto).
-description('N2O Protocol WebSocket endpoint handler').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

formatter(O)-> case lists:keyfind(formatter,1,O) of {formatter,F} -> F; X -> X end.
upack(D)    -> binary_to_term(D,[safe]).
protocols() -> wf:config(n2o,protocols,[ n2o_heart,
                                         n2o_nitrogen,
                                         n2o_file,
                                         n2o_client,
                                         n2o_http ]).

terminate(_,#cx{module=Module}) -> catch Module:event(terminate).
init(_Transport, Req, _Opts, _) ->
    wf:actions([]),
    Ctx = (wf:init_context(Req))#cx{formatter=formatter(_Opts)},
    NewCtx = wf:fold(init,Ctx#cx.handlers,Ctx),
    wf:context(NewCtx),
    wf:reg(broadcast,{wf:peer(Req)}),
    {Origin, _} = cowboy_req:header(<<"origin">>, Req, <<"*">>),
    ConfigOrigin = wf:to_binary(wf:config(n2o,origin,Origin)),
    wf:info(?MODULE,"Origin: ~p",[ConfigOrigin]),
    Req1 = wf:header(<<"Access-Control-Allow-Origin">>, ConfigOrigin, NewCtx#cx.req),
    {ok, Req1, NewCtx}.

% N2O top level protocol NOP REPLY PUSH

info(M,R,S)               -> filter(M,R,S,protocols(),[]).
stream({text,_}=M,R,S)    -> filter(M,R,S,protocols(),[]);
stream({binary,<<>>},R,S) -> nop(R,S);
stream({binary,D},R,S)    -> filter(upack(D),R,S,protocols(),[]);
stream(_,R,S)             -> nop(R,S).

filter(M,R,S,Protos,Acc)  -> case application:get_env(n2o,filter,{?MODULE,push}) of
                                  undefined -> push(M,R,S,Protos,Acc);
                                  {Mod,Fun} -> Mod:Fun(M,R,S,Protos,Acc) end.

nop(R,S)                  -> {reply,<<>>,R,S}.
reply(M,R,S)              -> {reply,M,R,S}.
push(_,R,S,[],_)          -> nop(R,S);
push(M,R,S,[H|T],Acc)     ->
    case H:info(M,R,S) of
         {unknown,_,_,_}  -> push(M,R,S,T,Acc);
         {reply,M1,R1,S1} -> reply(M1,R1,S1);
                        A -> push(M,R,S,T,[A|Acc]) end.
