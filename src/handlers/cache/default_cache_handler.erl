% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_cache_handler).
-behaviour (cache_handler).
-export ([
    init/2, 
    finish/2,
    get_cached/5,
    clear/3, 
    clear_all/2
]).

init(_Config, State) -> 
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

get_cached(_Key, Function, _TTL, _Config, State) -> 
    {ok, Function(), State}.

clear(_Key, _Config, State) -> 
    {ok, State}.

clear_all(_Config, State) -> 
    {ok, State}.
