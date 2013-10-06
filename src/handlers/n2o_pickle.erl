-module(n2o_pickle).
-include_lib("n2o/include/wf.hrl").
-export(?PICKLES_API).

pickle(Data) -> base64:encode(term_to_binary({Data, now()}, [compressed])).
secret() -> wf:config(secret,"n2o").

depickle(PickledData) ->
    try {Data, _PickleTime} = inner_depickle(PickledData), Data
    catch _:_ -> undefined end.

depickle(PickledData, TTLSeconds) ->
    try {Data, PickledTime} = inner_depickle(PickledData),
        AgeInSeconds = timer:now_diff(now(), PickledTime) / 1024 / 1024,
        case AgeInSeconds < TTLSeconds of true -> Data; false -> undefined end
    catch _:_ -> undefined end.

inner_depickle(PickledData) ->
    try  {_Data, _PickleTime} = binary_to_term(base64:decode(wf:to_binary(PickledData)))
    catch _:_ -> undefined end.
