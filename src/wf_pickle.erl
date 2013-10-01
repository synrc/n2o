-module(wf_pickle).
-author('Rusty Klophaus').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

pickle(Data) ->
    B = term_to_binary({Data, now()}, [compressed]),
    <<Signature:4/binary, _/binary>> = B,
    base64:encode(<<Signature/binary, B/binary>>).

depickle(PickledData) ->
    try {Data, _PickleTime} = inner_depickle(PickledData), Data
    catch _:_ -> undefined end.

depickle(PickledData, TTLSeconds) ->
    try {Data, PickledTime} = inner_depickle(PickledData),
        AgeInSeconds = timer:now_diff(now(), PickledTime) / 1024 / 1024,
        case AgeInSeconds < TTLSeconds of true -> Data; false -> undefined end
    catch _:_ -> undefined end.

inner_depickle(PickledData) ->
    try <<S:4/binary, B/binary>> = base64:decode(wf:to_binary(PickledData)),
        {_Data, _PickleTime} = binary_to_term(B)
    catch _:_ -> undefined end.
