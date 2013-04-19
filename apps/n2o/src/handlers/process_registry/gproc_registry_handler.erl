-module(gproc_registry_handler).
-behaviour(process_registry_handler).
-include_lib("n2o/include/wf.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([init/2, finish/2, get_pid/3, get_pid/4]).

init(_Config, State) -> {ok, State}.
finish(_Config, State) -> {ok, State}.

get_pid(Key, _Config, State) -> 
    PList = qlc:e(qlc:q([P || {{p,l,K},P,Val} <- gproc:table(props), K == Key ])),
    Pid = lists:nth(1,PList),
    error_logger:info_msg("GProc Lookup: ~p ~p~n",[Key,Pid]),
    {ok, Pid, State}.

get_pid(Key, Function, _Config, State) ->
    Pid = erlang:spawn_link(fun() -> closure(Key,Function) end), 
    error_logger:info_msg("GProc Create: ~p ~p ~p~n",[Key,Function,Pid]),
    {ok, Pid, State}.

closure(Key,Function) -> gproc:reg({p,l,Key},self()), Function().
