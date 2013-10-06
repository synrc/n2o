-module(n2o_cowboy).
-author('Roman Shestakov').
-behaviour(cowboy_http_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/3, handle/2, terminate/3]).
-compile(export_all).
-record(state, {headers, body}).

% Cowbot HTTP Handler

init(_Transport, Req, Opts) -> {ok, Req, #state{}}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) -> {ok, NewReq} = wf_core:run(Req), {ok, NewReq, State}.

% Cowboy Bridge Abstraction

params(Req) -> {Params,NewReq} = cowboy_req:qs_vals(Req), Params.
path(Req) -> {Path,NewReq} = cowboy_req:path(Req), Path.
request_body(Req) -> cowboy_req:body(Req).
headers(Req) -> cowboy_req:headers(Req).
header(Name, Value, Req) -> cowboy_req:set_resp_header(Name, Value, Req).
response(Html,Req) -> cowboy_req:set_resp_body(Html,Req).
reply(StatusCode,Req) -> cowboy_req:reply(StatusCode, Req).
cookies(Req) -> {Cookies,Req} = cowboy_req:cookies(Req), Cookies.
cookie(Cookie,Req) when is_atom(Cookie) -> cookie(list_to_binary(atom_to_list(Cookie)),Req);
cookie(Cookie,Req) -> {Val,_} = cowboy_req:cookie(Cookie,Req), Val.
cookie(Cookie, Value, Req) -> cookie(Cookie,Value,"/",0,Req).
cookie(Name, Value, Path, TTL, Req) ->
    Options = [{path, Path}, {max_age, TTL}],
    cowboy_req:set_resp_cookie(Name, Value, Options, Req).
delete_cookie(Cookie,Req) -> cookie(Cookie,"","/",0,Req).
peer(Req) -> {{Ip,Port},Req} = cowboy_req:peer(Req), {Ip,Port}.
