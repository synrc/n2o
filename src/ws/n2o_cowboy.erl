-module(n2o_cowboy).
-include_lib("n2o/include/n2o.hrl").
-description('N2O Cowboy HTTP Backend').
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3, points/0]).
-export([params/1, form/1, path/1, request_body/1, headers/1, header/3,
         response/2, reply/2, cookies/1, cookie/2, cookie/3, cookie/5, delete_cookie/2,
         peer/1, env/1, fix2/1, fix1/1]).

% Cowboy 2 WebSocket API

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

% Cowboy Bridge Abstraction

params(Req) -> cowboy_req:qs_vals(Req).
form(Req) -> {ok,Params,NewReq} = cowboy_req:body_qs(Req), {Params,NewReq}.
path(Req) -> {Path,_NewReq} = cowboy_req:path(Req), Path.
request_body(Req) -> cowboy_req:body(Req).
headers(Req) -> cowboy_req:headers(Req).
header(Name, Value, Req) -> cowboy_req:set_resp_header(Name, Value, Req).
response(Html,Req) -> cowboy_req:set_resp_body(Html,Req).
reply(StatusCode,Req) -> cowboy_req:reply(StatusCode, Req).
cookies(Req) -> element(1,cowboy_req:cookies(Req)).
cookie(Cookie,Req) -> element(1,cowboy_req:cookie(n2o:to_binary(Cookie),Req)).
cookie(Cookie, Value, Req) -> cookie(Cookie,Value,<<"/">>,0,Req).
cookie(Name, Value, Path, TTL, Req) ->
    Options = [{path, Path}, {max_age, TTL}],
    cowboy_req:set_resp_cookie(Name, Value, Options, Req).
delete_cookie(Cookie,Req) -> cookie(Cookie,<<"">>,<<"/">>,0,Req).
peer(Req) -> {{Ip,Port},Req} = cowboy_req:peer(Req), {Ip,Port}.

fix1({error,bad_name}) -> "priv";
fix1(X) -> case filelib:is_dir(X) of true -> X; _ -> fix1({error,bad_name}) end.
fix2({error,bad_name}) -> "deps/n2o/priv";
fix2(X) -> case filelib:is_dir(X) of true -> X; _ -> fix2({error,bad_name}) end.

static() -> { dir, fix1(code:priv_dir(application:get_env(n2o,app,review)))++"/static", mime() }.
n2o()    -> { dir, fix2(code:priv_dir(n2o)), mime() }.
mime()   -> [ { mimetypes, cow_mimetypes, all } ].
port()   -> application:get_env(n2o,port,8000).

points() -> cowboy_router:compile([{'_', [
            { "/n2o/[...]", cowboy_static,  n2o()      },
            { "/app/[...]", cowboy_static,  static()   },
            { "/ws/[...]",  n2o_cowboy,  []            } ]}]).

env(App) -> [{port,       port()},
             {certfile,   code:priv_dir(App)++"/ssl/fullchain.pem"},
             {keyfile,    code:priv_dir(App)++"/ssl/privkey.pem"},
             {cacertfile, code:priv_dir(App)++"/ssl/fullchain.pem"}].

