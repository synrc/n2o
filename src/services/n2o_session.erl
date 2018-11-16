-module(n2o_session).
-description('N2O In-Memory Sessions').
-compile(export_all).

authenticate(ClientSessionId, ClientSessionToken) ->
    n2o:info(?MODULE, "Session Init ~nClientId ~p: Token ~p~n~n", [ClientSessionId, ClientSessionToken]),
    Expiration = till(calendar:local_time(), ttl()),
    Response = case ClientSessionToken of
        [] ->
            NewSID = generate_sid(),
            ClientToken = encode_token(NewSID),
            Token = {{NewSID,<<"auth">>},os:timestamp(),Expiration},
            ets:insert(cookies,Token),
            n2o:info(?MODULE, "Auth Token New: ~p~n~p~n~n", [Token, ClientToken]),
            {'Token', ClientToken};
        ExistingToken ->
            SessionId = decode_token(ClientSessionToken),
            case SessionId of
                [] -> {error, "Invalid token signature"};
                _Val ->
                 Lookup = lookup_ets({SessionId,<<"auth">>}),
                 InnerResponse = case Lookup of
                    [] -> {error, "Invalid authentication token"};
                    {{TokenValue,Key},Issued,Till} ->
                        case expired(Issued,Till) of
                            false ->
                                Token = {{TokenValue,Key},Issued,Till},
                                io:format("Auth Token Ok: ~p~n", [Token]),
                                {'Token', ExistingToken};
                            true ->
                                UpdatedSID = generate_sid(),
                                UpdatedClientToken = encode_token(UpdatedSID),
                                Token = {{UpdatedSID,<<"auth">>},os:timestamp(),Expiration},
                                delete_old_token({TokenValue,<<"auth">>}),
                                ets:insert(cookies,Token),
                                n2o:info(?MODULE, "Auth Token Expired: ~p~nGenerated new token ~p~n", [TokenValue, Token]),
                                {'Token', UpdatedClientToken} end;
                    What -> n2o:info(?MODULE, "Auth Cookie Error: ~p~n",[What]), {fail, What} end,
                 InnerResponse end
        end,
    Response.

encode_token(Data) ->
    n2o_secret:pickle(Data).

decode_token(Data) ->
    Res = n2o_secret:depickle(Data),
    case Res of
        <<>> -> [];
        Value -> Value end.

expired(_Issued,Till) -> Till < calendar:local_time().

lookup_ets(Key) ->
    Res = ets:lookup(cookies,Key),
    case Res of
         [] -> [];
         [Value] -> Value;
         Values -> Values end.

delete_old_token(Session) ->
    ets:delete_object(cookies, lookup_ets(Session)).

ttl() -> application:get_env(n2o,ttl,60*15).

till(Now,TTL) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + TTL).

generate_sid() ->
    nitro_conv:hex(binary:part(crypto:hmac(application:get_env(n2o,hmac,sha256),
         n2o_secret:secret(),term_to_binary(os:timestamp())),0,16)).

invalidate_sessions() ->
    ets:foldl(fun(X,A) -> {Sid,Key} = element(1,X), get_value(Sid,Key,[]), A end, 0, cookies).

get_value(SID, Key, DefaultValue) ->
    Res = case lookup_ets({SID,Key}) of
               [] -> DefaultValue;
               {{SID,Key},_,Issued,Till,Value} -> case expired(Issued,Till) of
                       false -> Value;
                       true -> ets:delete(cookies,{SID,Key}), DefaultValue end end,
    Res.

set_value(SID, Key, Value) ->
    NewTill = till(calendar:local_time(), ttl()),
    ets:insert(cookies,{{SID,Key},<<"/">>,os:timestamp(),NewTill,Value}),
    Value.

positive_test() ->
  {'Token',B}=n2o_session:authenticate("",""),
  32=size(n2o_session:decode_token(B)),
  {'Token',C}=n2o_session:authenticate("",B),
  %  need to delete all test data
  delete_old_token({decode_token(C),<<"auth">>}),
  true=(C==B).

negative_test1() ->
    InputValue = os:timestamp(),
    {error, Reason} = n2o_session:authenticate("", InputValue),
    Reason=="Invalid token signature".

negative_test2() ->
    InputValue = n2o_secret:pickle(os:timestamp()),
    {error, Reason} = n2o_session:authenticate("", InputValue),
    Reason=="Invalid authentication token".

negative_test3() ->
    application:set_env(n2o, ttl, 2),
    {'Token', TokenA} = n2o_session:authenticate("", ""),
    timer:sleep(3000),
    {'Token', TokenB} = n2o_session:authenticate("", TokenA),
    application:set_env(n2o, ttl, 60*15),
    TokenWasChanged = TokenA/=TokenB,
    {'Token', TokenC} = n2o_session:authenticate("", TokenB),
    NewTokenIsValid = TokenB == TokenC,
    delete_old_token({decode_token(TokenC),<<"auth">>}),
    TokenWasChanged == NewTokenIsValid.

test_set_get_value() ->
    InputValue = base64:encode(crypto:strong_rand_bytes(8)),
    SID = base64:encode(crypto:strong_rand_bytes(8)),
    Key = base64:encode(crypto:strong_rand_bytes(8)),
    set_value(SID, Key, InputValue),
    ResultValue = get_value(SID, Key, []),
    delete_old_token({SID,Key}),
    InputValue == ResultValue.

% TODO:
% 1. plug n2o:session API to cookies ETS
% 2. session invalidation by timer
% 3. moar tests

