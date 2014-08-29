-module(wf_utils).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

f(S) -> f(S, []).
f(S, Args) -> lists:flatten(io_lib:format(S, Args)).

guid() ->
    MD5 = erlang:md5(term_to_binary({node(), now(), make_ref()})),
    MD5List = lists:nthtail(8, binary_to_list(MD5)),
    F = fun(N) -> wf:f("~2.16.0B", [N]) end,
    L = [F(N) || N <- MD5List],
    lists:flatten(L).

short_guid() ->
    MD5 = erlang:md5(term_to_binary({node(), now(), make_ref()})),
    MD5List = lists:nthtail(14, binary_to_list(MD5)),
    F = fun(N) -> wf:f("~2.16.0B", [N]) end,
    L = [F(N) || N <- MD5List],
    lists:flatten(L).

replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
    Length = length(S1),
    case string:substr(String, 1, Length) of
        S1 -> S2 ++ replace(string:substr(String, Length + 1), S1, S2);
        _ -> [hd(String)|replace(tl(String), S1, S2)]
    end.

coalesce([]) -> undefined;
coalesce([H]) -> H;
coalesce([undefined|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.

indexof(Key, Fields) -> indexof(Key, Fields, 2).
indexof(_Key, [], _N) -> undefined;
indexof(Key, [Key|_T], N) -> N;
indexof(Key, [_|T], N) -> indexof(Key, T, N + 1).

hunmap([],O,_,_) -> O;
hunmap([{BK,V}|T],O,Keys,0) -> O;
hunmap([{BK,V}|T],O,Keys,L) ->
    K = wf:to_atom(BK),
    hunmap(T, setelement(wf_utils:indexof(K,Keys),O,wf:to_list(V)), Keys--[K],L-1).

is_char(C) -> is_integer(C) andalso C >= 0 andalso C =< 255.

is_string([N | _] = PossibleString) when is_number(N) -> lists:all(fun is_char/1, PossibleString);
is_string(_)                                          -> false.

config(App, Key, Default) -> case application:get_env(App,Key) of
                                undefined -> Default;
                                {ok,V} -> V end.

os_env(Key) -> os_env(Key, "").
os_env(Key, Default) when is_atom(Key) ->
    os_env(string:to_upper(atom_to_list(Key)), Default);
os_env(Key, Default) ->
    case os:getenv(Key) of false -> Default; V -> V end.
