#!/usr/bin/env escript

% This script boots up the Reltool Server for apps ordering
% It also could generate reltool.config

-module(orderapps).
-compile([export_all]).

relconfig(Apps) ->
    LibDirs = [Dir || Dir <- ["apps", "deps"], case file:read_file_info(Dir) of {ok, _} -> true; _ -> false end],
    {sys, [{lib_dirs,LibDirs}, {rel,"node","1",Apps}, {profile, embedded},
           {boot_rel,"node"}, {app,observer,[{incl_cond,exclude}]} ]}.

main([]) -> io:format("usage: ./orderapps.erl apps~n", []);
main(MainApps) ->
    Relconfig = relconfig([list_to_atom(A) || A <- MainApps]),
    {ok, Server} = reltool:start_server([{config, Relconfig}]),
    {ok, {release, _Node, _Erts, Apps}} = reltool_server:get_rel(Server, "node"),
    Ordered = [element(1, A) || A <- Apps],
    io:format("~w~n", [Ordered]).
