-module(n2o_session).
-description('N2O Session').
-compile(export_all).

to(X)         -> calendar:datetime_to_gregorian_seconds(X).
from(X)       -> calendar:gregorian_seconds_to_datetime(X).
cut(Bin)      -> binary:part(Bin,0,20).
expired(Till) -> Till < to(calendar:local_time()).
expire()      -> to(till(calendar:local_time(), ttl())).
auth(Sid)     -> {{Sid,<<"auth">>},{expire(),[]}}.
new(Auth)     -> ets:insert(cookies,Auth), {'Token',n2o:pickle(Auth)}.
ttl()         -> application:get_env(n2o,ttl,60*15).
till(Now,TTL) -> from(to(Now)+TTL).

generate_sid() ->
    nitro_conv:hex(binary:part(crypto:hmac(application:get_env(n2o,hmac,sha256),
         n2o_secret:secret(),term_to_binary(os:timestamp())),0,16)).

authenticate([], Pickle) ->
    case n2o:depickle(Pickle) of
        <<>> -> new(auth(generate_sid()));
        {{Sid,<<"auth">>},{Till,[]}} = Auth ->
            case expired(Till) of
                false -> case application:get_env(n2o,nitro_prolongate,no) of
                              no -> new(Auth);
                               _ -> new(auth(Sid)) end;
                 true -> delete_auth({Sid,<<"auth">>}),
                         new(auth(generate_sid()))
            end
    end.

lookup_ets(Key) ->
    Res = ets:lookup(cookies,Key),
    case Res of
         [Value] -> Value;
         Values -> Values end.

delete_auth(Session) ->
    case lookup_ets(Session) of
         [] -> skip;
          X -> ets:delete_object(cookies, X) end.

invalidate_sessions() ->
    ets:foldl(fun(X,A) -> {Sid,Key} = element(1,X),
              get_value(Sid,Key,[]), A end, 0, cookies),ok.

get_value(SID, Key, DefaultValue) ->
    Res = case lookup_ets({SID,Key}) of
               [] -> DefaultValue;
               {{SID,Key},{Till,[]}} -> case expired(Till) of
                    false -> [];
                    true -> ets:delete(cookies,{SID,Key}) end;
               {{SID,Key},{Till,{_,Value}}} -> case expired(Till) of
                    false -> Value;
                    true -> ets:delete(cookies,{SID,Key}), DefaultValue end end,
    Res.

set_value(SID, Key, Value) ->
    ets:insert(cookies,{{SID,Key},{expire(),{<<"/">>,Value}}}),
    Value.

positive_test() ->
    application:set_env(n2o,nitro_prolongate,no),
    {'Token',B}=n2o_session:authenticate("",""),
    {{SID,Key},{Till,[]}} = n2o:depickle(B),
    {'Token',C}=n2o_session:authenticate("",B),
    {{SID,Key},{Till,[]}} = n2o:depickle(C),
    delete_auth({SID,<<"auth">>}),
    true=(C==B).

negative_test() ->
    application:set_env(n2o,nitro_prolongate,no),
    application:set_env(n2o, ttl, 2),
    {'Token', TokenA} = n2o_session:authenticate("", ""),
    {{SID0,_},{_,[]}} = n2o:depickle(TokenA),
    timer:sleep(3000),
    {'Token', TokenB} = n2o_session:authenticate("", TokenA),
    {{SID1,_},{_,[]}} = n2o:depickle(TokenB),
    application:set_env(n2o, ttl, 60*15),
    TokenWasChanged = TokenA /= TokenB,
    {'Token', TokenC} = n2o_session:authenticate("", TokenB),
    {{SID2,_},{_,[]}} = n2o:depickle(TokenC),
    NewTokenIsValid = TokenB == TokenC,
    delete_auth({SID0,<<"auth">>}),
    delete_auth({SID1,<<"auth">>}),
    delete_auth({SID2,<<"auth">>}),
    TokenWasChanged == NewTokenIsValid.

test_set_get_value() ->
    InputValue = base64:encode(crypto:strong_rand_bytes(8)),
    SID = generate_sid(),
    Key = base64:encode(crypto:strong_rand_bytes(8)),
    set_value(SID, Key, InputValue),
    ResultValue = get_value(SID, Key, []),
    delete_auth({SID,Key}),
    InputValue == ResultValue.
