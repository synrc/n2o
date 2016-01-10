-module(n2o_stream).
-description('N2O Stream Bridge to WebSocket or XHR channels').
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3,handle/2,info/3,terminate/3]).
-export([websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

% XHR

init(T,R,O)                        -> upgrade(cowboy_req:header(<<"upgrade">>,R),{T,R,O}).
websocket(R,<<"websocket">>)       -> {upgrade, protocol, cowboy_websocket};
websocket(R,_)                     -> down(reply([],R,501)).
upgrade({undefined,R},{T,R,O})     -> initialize(T,R,O);
upgrade({B,R},_) when is_binary(B) -> websocket(R,cowboy_bstr:to_lower(B)).

handle(R,S)                  -> body(cowboy_req:body(R),S).
info(M,R,S)                  -> xhr(n2o_proto:info(M,R,S)).
terminate(_,R,S)             -> n2o_proto:terminate(R,S).
initialize(T,R,O)            -> xhr(n2o_proto:init(T,R,[{formatter,json}|O],xhr)).

body({ok,D,R2},S)            -> xhr(n2o_proto:stream({type(D),D},R2,S));
body(R,S)                    -> {ok,R,S}.
down(R)                      -> {shutdown,R,undefined}.
reply(D,R,Code)              -> {ok,R2}=cowboy_req:reply(Code,[],D,R), R2.

type(<<"N2O,",_/binary>>)    -> text;
type(<<"PING">>)             -> text;
type(_)                      -> binary.

xhr({ok,R,S})                -> {ok,R,S};
xhr({shutdown,R,S})          -> {shutdown,R,S};
xhr({reply,D,R,S})           -> {ok,reply(D,R,200),S}.

% WebSocket

websocket_info(I,R,S)        -> ws(n2o_proto:info(I,R,S)).
websocket_handle(D,R,S)      -> ws(n2o_proto:stream(D,R,S));
websocket_handle(_,R,S)      -> {ok,R,S,hibernate}.
websocket_init(T,R,O)        -> ws(n2o_proto:init(T,R,[{formatter,bert}|O],ws)).
websocket_terminate(_,R,S)   -> n2o_proto:terminate(R,S).

ws({ok,R,S})                 -> {ok,R,S,hibernate};
ws({shutdown,R,S})           -> {shutdown,R,S};
ws({reply,{binary,Rep},R,S}) -> {reply,{binary,Rep},R,S,hibernate};
ws({reply,Rep,R,S})          -> {reply,{text,Rep},R,S,hibernate}.
