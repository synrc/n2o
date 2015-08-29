-module(n2o_secret).
-author('Oleksandr Nikitin').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).
-export(?PICKLES_API).

pickle(Data) ->
    Message = term_to_binary({Data,os:timestamp()}),
    Padding = size(Message) rem 16,
    Bits = (16-Padding)*8, Key = secret(), IV = crypto:rand_bytes(16),
    Cipher = crypto:block_encrypt(aes_cbc128,Key,IV,<<Message/binary,0:Bits>>),
    Signature = crypto:hmac(sha256,Key,<<Cipher/binary,IV/binary>>),
    base64:encode(<<IV/binary,Signature/binary,Cipher/binary>>).

secret() -> wf:config(n2o,secret,<<"ThisIsClassified">>).

depickle(PickledData) ->
    try Key = secret(),
        Decoded = base64:decode(wf:to_binary(PickledData)),
        <<IV:16/binary,Signature:32/binary,Cipher/binary>> = Decoded,
        Signature = crypto:hmac(sha256,Key,<<Cipher/binary,IV/binary>>),
        {Data,_Time} = binary_to_term(crypto:block_decrypt(aes_cbc128,Key,IV,Cipher),[safe]),
        Data
    catch E:R -> wf:info(?MODULE,"Depicke Error: ~p",[{E,R}]), undefined end.
