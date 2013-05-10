-module(default_cache_handler).
-author('Rusty Klophaus').
-behaviour (cache_handler).
-export([init/2, finish/2, get_cached/5, clear/3, clear_all/2]).

init(_Config, State) -> {ok, State}.
finish(_Config, State) -> {ok, State}.
get_cached(_Key, Function, _TTL, _Config, State) -> {ok, Function(), State}.
clear(_Key, _Config, State) -> {ok, State}.
clear_all(_Config, State) -> {ok, State}.
