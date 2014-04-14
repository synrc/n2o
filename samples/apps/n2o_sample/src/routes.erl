-module (routes).
-author('Maxim Sokhatsky').
-behaviour (route_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
    {ok, State, Ctx#context{path=Path,module=route(Path)}}.

route(<<"/">>) -> login;
route(<<"/index">>) -> index;
route(<<"/login">>) -> login;
route(<<"/lispchat">>) -> lispchat;
route(<<"/ws/">>) -> login;
route(<<"/ws/lispchat">>) -> lispchat;
route(<<"/ws/index">>) -> index;
route(<<"/ws/login">>) -> login;
route(<<"/ws/static/spa/spa.htm">>) -> login;
route(<<"/ws/static/spa/index.htm">>) -> index;
route(<<"/favicon.ico">>) -> static_file;
route(_) -> index.
