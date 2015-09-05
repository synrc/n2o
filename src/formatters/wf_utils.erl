-module(wf_utils).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

f(S) -> f(S, []).
f(S, Args) -> lists:flatten(io_lib:format(S, Args)).

replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
    Length = length(S1),
    case string:substr(String, 1, Length) of
        S1 -> S2 ++ replace(string:substr(String, Length + 1), S1, S2);
        _ -> [hd(String)|replace(tl(String), S1, S2)]
    end.

coalesce({X,Y}) -> Y;
coalesce(false) -> undefined;
coalesce([]) -> undefined;
coalesce([H]) -> H;
coalesce([undefined|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.

indexof(Key, Fields) -> indexof(Key, Fields, 2).
indexof(_Key, [], _N) -> undefined;
indexof(Key, [Key|_T], N) -> N;
indexof(Key, [_|T], N) -> indexof(Key, T, N + 1).

config(App, Key, Default) -> application:get_env(App,Key,Default).

os_env(Key) -> os_env(Key, "").
os_env(Key, Default) -> case os:getenv(Key) of false -> Default; V -> V end.
