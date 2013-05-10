-module(config_handler).
-author('Rusty Klophaus').
-export([behaviour_info/1, get_value/1, get_values/1, get_value/2, get_values/2]).

get_value(Key) -> get_value(Key, undefined).
get_value(Key, DefaultValue) -> wf_handler:call_readonly(config_handler, get_value, [Key, DefaultValue]).
get_values(Key) -> get_values(Key, undefined).
get_values(Key, DefaultValue) -> wf_handler:call_readonly(config_handler, get_values, [Key, DefaultValue]).

behaviour_info(callbacks) -> [
    {init, 2},      
    {finish, 2},
    {get_value, 4},
    {get_values, 4}
];

behaviour_info(_) -> undefined.
