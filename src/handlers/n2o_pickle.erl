-module(n2o_pickle).
-include_lib("n2o/include/wf.hrl").
-export(?PICKLES_API).

pickle(Data) -> base64:encode(term_to_binary({Data, os:timestamp()}, [compressed])).
depickle(PickledData) ->
    try {Data, _PickleTime} = binary_to_term(base64:decode(wf:to_binary(PickledData))), Data
    catch _:_ -> undefined end.
