-module(n2o_proto).
-description('N2O Proto Loop').
-include("n2o.hrl").
-compile(export_all).
-export([info/3, stream/3, push/5]).

formatter(O)       -> case lists:keyfind(formatter,1,O) of {formatter,F} -> F; X -> X end.
protocols()        -> application:get_env(n2o,protocols,[ n2o_nitro ]).
info(M,R,S)        -> filter(M,R,S,protocols(),[]).
filter(M,R,S,P,A)  -> {Mod,Fun} = (application:get_env(n2o,filter,{?MODULE,push})),
                      put(context,S),
                      Mod:Fun(M,R,S,P,A).

nop(R,S)                  -> {reply,{binary,<<>>},R,S}.
reply(M,R,S)              -> {reply,M,R,S}.
push(_,R,S,[],_)          -> nop(R,S);
push(M,R,S,[H|T],Acc)     ->
    case H:info(M,R,S) of
         {unknown,_,_,_}  -> push(M,R,S,T,Acc);
         {reply,M1,R1,S1} -> reply(M1,R1,S1);
                        A -> push(M,R,S,T,[A|Acc]) end.

cx(Req) -> #cx{actions=[], path=[], req=Req, params=[],
               handlers= [ {routes, application:get_env(n2o,routes,routes)} ]}.

fold(Fun,Handlers,Ctx) ->
    lists:foldl(
        fun ({_,[]},C) -> C;
            ({_,Module},Ctx1) ->
             {ok,_,NewCtx} = Module:Fun([],Ctx1), NewCtx
        end, Ctx, Handlers).

terminate(_,#cx{module=Module}) -> catch Module:event(terminate).
init(_Transport, Req, _Opts, _) ->
    Zero = cx(Req),
    Ctx  = fold(init,Zero#cx.handlers,Zero),
    put(context,Ctx),
    Origin = case cowboy_req:header(<<"origin">>, Req, <<"*">>) of {O,_} -> O; X -> X end,
    ConfigOrigin = iolist_to_binary(application:get_env(n2o,origin,Origin)),
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, ConfigOrigin, Ctx#cx.req),
    {ok, Req1, Ctx}.

upack(D)                  -> n2o:decode(D).
stream({text,_}=M,R,S)    -> filter(M,R,S,protocols(),[]);
stream({binary,<<>>},R,S) -> nop(R,S);
stream({binary,D},R,S)    -> filter(upack(D),R,S,protocols(),[]);
stream(_,R,S)             -> nop(R,S).
