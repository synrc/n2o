-module (n2o_route).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?ROUTING_API).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
    {ok, State, Ctx#context{path=Path,module=route(Path)}}.

route(<<"/">>) -> index;
route(<<"/index">>) -> index;
route(<<"/login">>) -> login;
route(<<"/ws/">>) -> index;
route(<<"/ws/index">>) -> index;
route(<<"/ws/login">>) -> login;
route(<<"/favicon.ico">>) -> static_file;
route(_) -> index.
