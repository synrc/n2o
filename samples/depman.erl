#!/usr/bin/env escript

-module(depman).
-compile([export_all]).
-define(ABORT(Str, Args), io:format(Str, Args), throw(abort)).

app_exists(App,Srv) when is_tuple(App) -> app_exists(element(1,App), Srv);
app_exists(App,Srv) when is_atom(App) -> case reltool_server:get_app(Srv,App) of {ok, _} -> true; _ -> false end.

validate_rel_apps(ReltoolServer, {sys, ReltoolConfig}) ->
    case lists:keyfind(rel, 1, ReltoolConfig) of
        false -> ok;
        {rel, _Name, _Vsn, Apps} ->
            Missing = lists:sort([App || App <- Apps, app_exists(App, ReltoolServer) == false]),
            case Missing of [] -> ok; _ -> ?ABORT("Missing Apps: ~p\n", [Missing]) end;
        Rel -> ?ABORT("Invalid {rel, ...} section in reltool.config: ~p\n", [Rel]) end.

relconfig(Apps) ->
    LibDirs = [Dir || Dir <- ["apps", "deps"], case file:read_file_info(Dir) of {ok, _} -> true; _ -> false end],
    {sys, [ {lib_dirs,LibDirs}, {rel,"node","1",Apps}, {boot_rel,"node"}, {app,observer,[{incl_cond,exclude}]} ]}.

main([]) -> ?ABORT("usage: ./depman.erl apps", []);
main(MainApps) ->
    Relconfig = relconfig([list_to_atom(A) || A <- MainApps]),
    {ok, Server} = reltool:start_server([{config, Relconfig}]),
    validate_rel_apps(Server, Relconfig),
    {ok, {release, _Node, _Erts, Apps}} = reltool_server:get_rel(Server, "node"),
    Alist = [element(1, A) || A <- Apps],
    io:format("~w~n", [Alist]).
