-module(n2o_secret).
-description('N2O HMAC AES/CBC-128').
-include("n2o.hrl").
-export([pickle/1,depickle/1,secret/0]).

pickle(Data) ->
    Message = term_to_binary({Data,os:timestamp()}),
    Padding = size(Message) rem 16,
    Bits = (16-Padding)*8, Key = secret(), IV = crypto:strong_rand_bytes(16),
    Cipher = crypto:block_encrypt(aes_cbc128,Key,IV,<<Message/binary,0:Bits>>),
    Signature = crypto:hmac(sha256,Key,<<Cipher/binary,IV/binary>>),
    nitro_conv:hex(<<IV/binary,Signature/binary,Cipher/binary>>).

secret() -> application:get_env(n2o,secret,<<"ThisIsClassified">>).

depickle(PickledData) ->
    try Key = secret(),
        Decoded = nitro_conv:unhex(iolist_to_binary(PickledData)),
        <<IV:16/binary,Signature:32/binary,Cipher/binary>> = Decoded,
        Signature = crypto:hmac(sha256,Key,<<Cipher/binary,IV/binary>>),
        {Data,_Time} = binary_to_term(crypto:block_decrypt(aes_cbc128,Key,IV,Cipher),[safe]),
        Data
    catch _:_ -> <<>> end.
