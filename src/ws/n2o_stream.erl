-module(n2o_stream).
-description('N2O Cowboy WebSocket Backend').
-export([init/3,handle/2,info/3,terminate/3]).
-export([websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init(T,R,O)                        -> upgrade(cowboy_req:header(<<"upgrade">>,R),{T,R,O}).
upgrade({undefined,R},{T,R,O})     -> xhr(n2o_proto:init(T,R,[{formatter,json}|O],xhr));
upgrade({B,R},_) when is_binary(B) -> websocket(R,cowboy_bstr:to_lower(B)).
terminate(_,R,S)                   -> n2o_proto:terminate(R,S).

% Lib

body({ok,D,R2},S) -> xhr(n2o_proto:stream({type(D),D},R2,S));
body(R,S)         -> {ok,R,S}.
down(R)           -> {shutdown,R,undefined}.

type(<<"N2O,",_/binary>>)    -> text;
type(<<"PING">>)             -> text;
type(_)                      -> binary.

% Cowboy XHR

handle(R,S) -> body(cowboy_req:body(R),S).
info(M,R,S) -> xhr(n2o_proto:info(M,R,S)).

reply(D,R,Code)     -> {ok,R2}=cowboy_req:reply(Code,[],D,R), R2.

xhr({ok,R,S})       -> {ok,R,S};
xhr({reply,D,R,S})  -> {ok,reply(D,R,200),S}.

% Cowboy WebSocket

websocket(_,<<"websocket">>) -> {upgrade, protocol, cowboy_websocket};
websocket(R,_) -> down(reply([],R,501)).

websocket_info(I,R,S)        -> ws(n2o_proto:info(I,R,S)).
websocket_handle(D,R,S)      -> ws(n2o_proto:stream(D,R,S)).
websocket_init(T,R,_)        -> ws(n2o_proto:init(T,R,[],ws)).
websocket_terminate(_,R,S)   -> n2o_proto:terminate(R,S).

ws({ok,R,S})                 -> {ok,R,S,hibernate};
ws({reply,{binary,Rep},R,S}) -> {reply,{binary,Rep},R,S,hibernate};
ws({reply,{json,Rep},R,S})   -> {reply,{binary,n2o_json:encode(Rep)},R,S,hibernate};
ws({reply,{bert,Rep},R,S})   -> {reply,{binary,n2o_bert:encode(Rep)},R,S,hibernate};
ws({reply,{text,Rep},R,S})   -> {reply,{text,Rep},R,S,hibernate};
ws({reply,{default,Rep},R,S})-> {reply,{binary,n2o:encode(Rep)},R,S,hibernate};
ws({reply,{Encoder,Rep},R,S})-> {reply,{binary,Encoder:encode(Rep)},R,S,hibernate};
ws(X) -> io:format("UNKNOWN: ~p~n",[X]).