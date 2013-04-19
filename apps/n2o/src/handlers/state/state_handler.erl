% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (state_handler).
-export ([
    behaviour_info/1,
    get_state/1, get_state/2, set_state/2, clear/1, clear_all/0
]).



% get_state(Key, State) -> Value.
% Retrieve a value from the storage area.
get_state(Key) -> 
    _Value = get_state(Key, undefined).

% get_state(Key, DefaultValue, State) -> Value.
% Retrieve a value from the storage area.
get_state(Key, DefaultValue) ->
    _Value = wf_handler:call_readonly(state_handler, get_state, [Key, DefaultValue]).

% set_state(Key, Value, State) -> {ok, NewState}.
% Put a value into the storage area.
set_state(Key, Value) ->
    ok = wf_handler:call(state_handler, set_state, [Key, Value]).

% clear(Key, State) -> {ok, NewState}.
% Remove a value from the storage area.
clear(Key) ->
    ok = wf_handler:call(state_handler, clear, [Key]).

% clear_all(State) -> {ok, NewState}.
% Clear all values from the storage area.
clear_all() ->
    ok = wf_handler:call(state_handler, clear_all).



behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_state, 4},
    {set_state, 4},
    {clear, 3},
    {clear_all, 2}
];

behaviour_info(_) -> undefined.
