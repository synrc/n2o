-module(n2o_cowboy).
-include("n2o.hrl").
-description('N2O Cowboy HTTP Backend').
-export([init/3, handle/2, terminate/3]).
-compile(export_all).
-record(state, {headers, body}).

% Cowboy HTTP Handler

init(_Transport, Req, _Opts) -> {ok, Req, #state{}}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) -> n2o:info(?MODULE,"DOCUMENT~n",[]), {ok, Req, State}.

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
cookie(Cookie,Req) -> element(1,cowboy_req:cookie(wf:to_binary(Cookie),Req)).
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
n2o() -> { dir, fix2(code:priv_dir(n2o)), mime() }.
mime() -> [ { mimetypes, cow_mimetypes, all } ].

points() -> cowboy_router:compile([{'_', [
            { "/n2o/[...]", cowboy_static,  n2o()      },
            { "/app/[...]", cowboy_static,  static()   },
            { "/ws/[...]",  n2o_stream,  []            } ]}]).
