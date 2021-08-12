-module(n2o_secret).
-description('N2O HMAC AES/CBC-128').
-include("n2o.hrl").
-export([pickle/1,depickle/1,hex/1,unhex/1,sid/1,mac/1]).

mac(Args) ->
  case erlang:list_to_integer(erlang:system_info(otp_release)) of
    X when X =< 22 -> erlang:apply(crypto,hmac,Args) ;
    _ -> erlang:apply(crypto,mac,[hmac]++Args)
  end.

pickle(Data) ->
    Message = term_to_binary(Data),
    Padding = size(Message) rem 16,
    Bits = (16-Padding)*8, Key = secret(), IV = crypto:strong_rand_bytes(16),
    Cipher = case erlang:list_to_integer(erlang:system_info(otp_release)) of
      X when X =< 22 -> erlang:apply(crypto,block_encrypt,[aes_cbc128,Key,IV,<<Message/binary,0:Bits>>]) ;
      _ -> erlang:apply(crypto,crypto_one_time,[aes_128_cbc,Key,IV,<<Message/binary,0:Bits>>,[{encrypt,true}]])
    end,
    Signature = mac([application:get_env(n2o,hmac,sha256),Key,<<Cipher/binary,IV/binary>>]),
    hex(<<IV/binary,Signature/binary,Cipher/binary>>).

depickle(PickledData) ->
    try Key = secret(),
        Decoded = unhex(iolist_to_binary(PickledData)),
        <<IV:16/binary,Signature:32/binary,Cipher/binary>> = Decoded,
        Signature = mac([application:get_env(n2o,hmac,sha256),Key,<<Cipher/binary,IV/binary>>]),
        Decipher = case erlang:list_to_integer(erlang:system_info(otp_release)) of
           X when X =< 22 -> binary_to_term(erlang:apply(crypto,block_decrypt,[aes_cbc128,Key,IV,Cipher]),[safe]) ;
           _ -> binary_to_term(erlang:apply(crypto,crypto_one_time,[aes_128_cbc,Key,IV,Cipher,[{encrypt,false}]]),[safe])
       end
    catch _:_ -> <<>> end.

secret() -> application:get_env(n2o,secret,<<"ThisIsClassified">>).
hex(Bin) -> << << (digit(A1)),(digit(A2)) >> || <<A1:4,A2:4>> <= Bin >>.
unhex(Hex) -> << << (erlang:list_to_integer([H1,H2], 16)) >> || <<H1,H2>> <= Hex >>.
digit(X) when X >= 0 andalso X =< 9 -> X + 48;
digit(X) when X >= 10 andalso X =< 15 -> X + 87.
sid(Seed) -> hex(binary:part(mac([application:get_env(n2o,hmac,sha256),secret(),term_to_binary(Seed)]),0,10)).
