-module(wf_pickle).
-author('Rusty Klophaus').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

pickle(Data) ->
    B = term_to_binary({Data, now()}, [compressed]),
    <<Signature:4/binary, _/binary>> = B, %erlang:md5([B, signkey()]),
    PickledData = base64:encode(<<Signature/binary, B/binary>>),
    PickledData.

depickle(PickledData) ->
    try {Data, _PickleTime} = inner_depickle(PickledData), Data
    catch _Type : _Message -> undefined end.

depickle(PickledData, TTLSeconds) ->
    try {Data, PickledTime} = inner_depickle(PickledData),
        AgeInSeconds = timer:now_diff(now(), PickledTime) / 1024 / 1024,
        case AgeInSeconds < TTLSeconds of true -> Data; false -> undefined end
    catch _Type : _Message -> undefined end.

signkey() ->
    SignKey = "1",%config_handler:get_value(signkey),
    case SignKey /= undefined of 
        true  -> SignKey;
        false -> Cookie = erlang:get_cookie(), erlang:md5(wf:to_list(Cookie)) end.

inner_depickle(PickledData) ->
    try <<S:4/binary, B/binary>> = base64:decode(wf:to_binary(PickledData)),
        <<S:4/binary, _/binary>> = B, %erlang:md5([B, signkey()]),
        {_Data, _PickleTime} = binary_to_term(B)
    catch _Type : _Message -> undefined end.
