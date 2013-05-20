-module(session_handler).
-author('Rusty Klophaus').
-export([behaviour_info/1, get_value/1, get_value/2, set_value/2]).

-define(SESSION, n2o_session_handler).

behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_value, 2},
    {set_value, 2}
];
behaviour_info(_) -> undefined.

get_value(Key) -> ?SESSION:get_value(Key,undefined).
get_value(Key, DefaultValue) -> ?SESSION:get_value(Key, DefaultValue).
set_value(Key, Value) -> ?SESSION:set_value(Key, Value).
