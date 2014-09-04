-module(n2o_dynroute).
-author('Andrey Martemyanov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).
-export(?ROUTING_API).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#cx.req),
    {ok, State, Ctx#cx{path=Path,module=route_prefix(Path)}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<"favicon.ico">>) -> static_file;
route(Route) -> % Just check that requested module is beam file
    Exist = lists:foldl(fun(L,A) ->
        M = filename:basename(L,".beam"),
        case A of
            undefined ->
                case { list_to_binary(M), 
                       lists:last(filename:split(filename:dirname(L))), 
                       filename:extension(L) } of
                     { Route,
                       "ebin",
                       ".beam" } -> {ok, M};
                    _ -> A
                end;
            _ -> A
        end end, undefined, filelib:wildcard("apps/*/ebin/*.beam")),
    case Exist of {ok, Module} -> list_to_atom(Module); _ -> login end.
