-module(routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

%% U can use default dynamic routes or define custom static as this
%% Just put needed module name to sys.config:
%% {n2o, [{route,routes}]}
%% Also with dynamic routes u must load all modules before starting Cowboy
%% [code:ensure_loaded(M) || M <- [index, login, ... ]]

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) ->
    Path = wf:path(Ctx#cx.req),
    wf:info(?MODULE,"Route: ~p~n",[Path]),
    {ok, State, Ctx#cx{path=Path,module=route_prefix(Path)}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>)              -> login;
route(<<"counter",_/binary>>) -> counter;
route(<<"chat",_/binary>>) -> chat;
route(<<"doc",_/binary>>) -> doc;
route(<<"index",_/binary>>) -> index;
route(<<"static/spa/index",_/binary>>) -> index;
route(<<"static/spa/login",_/binary>>) -> login;
route(<<"login",_/binary>>) -> login;
route(<<"interlogin",_/binary>>) -> interlogin;
route(_) -> login.
