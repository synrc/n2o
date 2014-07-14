-module(n2o_route).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?ROUTING_API).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
    {ok, State, Ctx#context{path=Path,module=route_prefix(Path)}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(Route) ->
    Exist = lists:foldl(fun({M,L},A) ->
        case A of
            undefined ->
                case {list_to_binary(atom_to_list(M)), lists:last(filename:split(filename:dirname(L))), filename:extension(L)} of
                    {Route, "ebin", ".beam"} -> {ok, M};
                    _ -> A
                end;
            _ -> A
        end end, undefined, code:all_loaded()),
    case Exist of {ok, Module} -> Module; _ -> index end.
