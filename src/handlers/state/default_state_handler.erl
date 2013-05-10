-module(default_state_handler).
-author('Rusty Klophaus').
-behaviour (state_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2, get_state/4, set_state/4, clear/3, clear_all/2]).

init(_Config, State) -> {ok, State}.

finish(_Config, State) -> {ok, State}.

get_state(Key, DefaultValue, _Config, State) -> 
    _Value = proplists:get_value(Key, State, DefaultValue).

set_state(Key, Value, _Config, State) ->
    State1 = proplists:delete(Key, State),
    State2 = [{Key, Value}|State1],
    {ok, State2}.

clear(Key, _Config, State) -> State1 = proplists:delete(Key, State), {ok, State1}.

clear_all(Config, _State) -> init(Config, []).
