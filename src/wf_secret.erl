-module(wf_secret).
-author('Oleksandr Nikitin').
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

pickle(Data) ->
    Message = term_to_binary({Data,now()}),
    Padding = size(Message) rem 16,
    Bits = (16-Padding)*8, Key = secret(), IV = crypto:rand_bytes(16),
    Cipher = crypto:block_encrypt(aes_cbc128,Key,IV,<<Message/binary,0:Bits>>),
    Signature = crypto:md5(<<Key/binary,Cipher/binary>>),
    base64:encode(<<IV/binary,Signature/binary,Cipher/binary>>).

secret() -> wf:config(n2o,secret,<<"ThisIsClassified">>).

depickle(PickledData) ->
    try {Data, _PickleTime} = inner_depickle(PickledData), Data
    catch _:_ -> undefined end.

depickle(PickledData, TTLSeconds) ->
    try {Data, PickledTime} = inner_depickle(PickledData),
        AgeInSeconds = timer:now_diff(now(), PickledTime) / 1024 / 1024,
        case AgeInSeconds < TTLSeconds of true -> Data; false -> undefined end
    catch _:_ -> undefined end.

inner_depickle(PickledData) ->
    try Key = secret(),
        Decoded = base64:decode(wf:to_binary(PickledData)),
        <<IV:16/binary,Signature:16/binary,Cipher/binary>> = Decoded,
        Signature = crypto:md5(<<Key/binary,Cipher/binary>>),
        binary_to_term(crypto:block_decrypt(aes_cbc128,Key,IV,Cipher))
    catch _:_ -> undefined end.
