-module(cache_handler).
-author('Rusty Klophaus').
-export([behaviour_info/1, get_cached/3, clear/1, clear_all/0]).

get_cached(Key, Function, TTL) -> {ok, _Value} = wf_handler:call(cache_handler, get_cached, [Key, Function, TTL]).
clear(Key) -> wf_handler:call(cache_handler, clear, [Key]).
clear_all() -> wf_handler:call(cache_handler, clear_all).

behaviour_info(callbacks) -> [
    {init, 2},
    {finish, 2},
    {get_cached, 5}, 
    {clear, 3},
    {clear_all, 2}
];
behaviour_info(_) -> undefined.
