-module(n2o_cowboy2).
-description('N2O Cowboy2 WebSocket Backend').
-include_lib("n2o/include/n2o.hrl").
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3,
         points/0]).

init(Req,_Opts) -> {cowboy_websocket, Req, Req}.

ws({ok,_,S})                 -> {ok,S};
ws({reply,{binary,Rep},_,S}) -> {reply,{binary,Rep},S};
ws({reply,{json,Rep},_,S})   -> {reply,{binary,n2o_json:encode(Rep)},S};
ws({reply,{bert,Rep},_,S})   -> {reply,{binary,n2o_bert:encode(Rep)},S};
ws({reply,{text,Rep},_,S})   -> {reply,{text,Rep},S};
ws({reply,{default,Rep},_,S})-> {reply,{binary,n2o:encode(Rep)},S};
ws({reply,{Encoder,Rep},_,S})-> {reply,{binary,Encoder:encode(Rep)},S};
ws(X) -> ?LOG_ERROR(#{unknown=>X}), {shutdown,[]}.

websocket_init(S)            -> ws(n2o_proto:init([],S,[],ws)).
websocket_handle(D,S)        -> ws(n2o_proto:stream(D,[],S)).
websocket_info(D,S)          -> ws(n2o_proto:info(D,[],S)).
terminate(M,R,S)             -> ws(n2o_proto:info({direct,{exit,M}},R,S)).

points() -> cowboy_router:compile([{'_', [
	    {"/ws/[...]", n2o_cowboy2, []},
        {"/n2o/[...]", cowboy_static, {dir, n2o_cowboy:fix2(code:priv_dir(n2o)), []}},
	    {"/app/[...]", cowboy_static, {dir, n2o_cowboy:fix1(code:priv_dir(
	                   application:get_env(n2o,app,review)))++"/static", []}} ]}]).
