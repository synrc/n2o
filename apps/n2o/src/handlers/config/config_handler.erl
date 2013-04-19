% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (config_handler).
-export ([
    behaviour_info/1,
    get_value/1, get_values/1,
    get_value/2, get_values/2
]).

% get_value(Application, Module, Key, DefaultValue, State) -> Value.
% Retrieve a configuration value.
get_value(Key) -> 
    _Value = get_value(Key, undefined).

get_value(Key, DefaultValue) -> 
    _Value = wf_handler:call_readonly(config_handler, get_value, [Key, DefaultValue]).

% get_values(Application, Module, Key, DefaultValue, State) -> Value.
% Retrieve a list of configuration values.
get_values(Key) -> 
    _Value = get_values(Key, undefined).

get_values(Key, DefaultValue) -> 
    _Value = wf_handler:call_readonly(config_handler, get_values, [Key, DefaultValue]).

behaviour_info(callbacks) -> [
    {init, 2},      
    {finish, 2},
    {get_value, 4},
    {get_values, 4}
];

behaviour_info(_) -> undefined.
