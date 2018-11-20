-module(n2o_session).
-include_lib("stdlib/include/ms_transform.hrl").
-description('N2O Session').
-compile(export_all).

% PRELUDE

part(X)       -> binary:part(X,0,10).
to(X)         -> calendar:datetime_to_gregorian_seconds(X).
from(X)       -> calendar:gregorian_seconds_to_datetime(X).
cut(Bin)      -> binary:part(Bin,0,20).
expired(Till) -> Till < to(calendar:local_time()).
expire()      -> to(till(calendar:local_time(), ttl())).
auth(Sid,Exp) -> {{Sid,<<"auth">>},{Exp,{[],[]}}}.
token(Auth)   -> ets:insert(cookies,Auth), {'Token',n2o:pickle(Auth)}.
ttl()         -> application:get_env(n2o,ttl,60*15).
till(Now,TTL) -> from(to(Now)+TTL).
prolongate()  -> application:get_env(n2o,nitro_prolongate,false).
sid(Seed)     -> nitro_conv:hex(binary:part(crypto:hmac(application:get_env(n2o,hmac,sha256),
                 n2o_secret:secret(),term_to_binary(Seed)),0,10)).

% API

authenticate([], Pickle) ->
    case n2o:depickle(Pickle) of
        <<>> -> token(auth(sid(os:timestamp()),expire()));
        {{Sid,<<"auth">>},{Till,{[],[]}}} ->
            case expired(Till) orelse prolongate() of
                false -> {'Token', Pickle};
                 true -> delete_auth({Sid,<<"auth">>}),
                         token(auth(Sid,expire())) end end.

get_value(Session, Key, Default) ->
    case lookup_ets({Session,Key}) of
         [] -> Default;
         {{Session,Key},{Till,{_,Value}}} -> case expired(Till) of
                false -> Value;
                true -> ets:delete(cookies,{Session,Key}), Default end end.

set_value(Session, Key, Value) ->
    ets:insert(cookies,{{Session,Key},{expire(),{<<"/">>,Value}}}), Value.

clear(Session) ->
    [ ets:delete(cookies,X) || X <- ets:select(cookies,
        ets:fun2ms(fun(A) when (element(1,element(1,A)) == Session) -> element(1,A) end)) ], ok.

lookup_ets(Key) ->
    case ets:lookup(cookies,Key) of [Value] -> Value; Values -> Values end.

delete_auth(Session) ->
    case lookup_ets(Session) of [] -> ok; X -> ets:delete_object(cookies, X) end.

invalidate_sessions() ->
    ets:foldl(fun(X,A) -> {Sid,Key} = element(1,X),
    get_value(Sid,Key,[]), A end, 0, cookies),ok.

% TESTS

positive_test() ->
    application:set_env(n2o,nitro_prolongate,false),
    {'Token',B}=n2o_session:authenticate("",""),
    {{SID,Key},{Till,{[],[]}}} = n2o:depickle(B),
    {'Token',C}=n2o_session:authenticate("",B),
    {{SID,Key},{Till,{[],[]}}} = n2o:depickle(C),
    delete_auth({SID,<<"auth">>}),
    true=(C==B).

negative_test() ->
    application:set_env(n2o,nitro_prolongate,false),
    application:set_env(n2o, ttl, 2),
    {'Token', TokenA} = n2o_session:authenticate("", ""),
    {{SID0,_},{_,{[],[]}}} = n2o:depickle(TokenA),
    timer:sleep(3000),
    {'Token', TokenB} = n2o_session:authenticate("", TokenA),
    {{SID1,_},{_,{[],[]}}} = n2o:depickle(TokenB),
    application:set_env(n2o, ttl, 60*15),
    TokenWasChanged = TokenA /= TokenB,
    {'Token', TokenC} = n2o_session:authenticate("", TokenB),
    {{SID2,_},{_,{[],[]}}} = n2o:depickle(TokenC),
    NewTokenIsValid = TokenB == TokenC,
    delete_auth({SID0,<<"auth">>}),
    delete_auth({SID1,<<"auth">>}),
    delete_auth({SID2,<<"auth">>}),
    TokenWasChanged == NewTokenIsValid.

test_set_get_value() ->
    InputValue = base64:encode(crypto:strong_rand_bytes(8)),
    SID = sid(os:timestamp()),
    Key = base64:encode(crypto:strong_rand_bytes(8)),
    set_value(SID, Key, InputValue),
    ResultValue = get_value(SID, Key, []),
    delete_auth({SID,Key}),
    InputValue == ResultValue.
