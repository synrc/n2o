-module(n2o_session).
-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
-description('N2O Session').
-export([authenticate/2, get_value/3, set_value/3, storage/0, prolongate/0, from/1, ttl/0, till/2]).
-export([clear/1, delete/1, update/1, lookup/1, invalidate_sessions/0]).

% PRELUDE

part(X)       -> binary:part(X,0,10).
to(X)         -> calendar:datetime_to_gregorian_seconds(X).
from(X)       -> calendar:gregorian_seconds_to_datetime(X).
cut(Bin)      -> binary:part(Bin,0,20).
expired(Till) -> Till < to(calendar:local_time()).
expire()      -> to(till(calendar:local_time(), ttl())).
auth(Sid,Exp) -> {{Sid,'auth'},{Exp,[]}}.
storage()     -> application:get_env(n2o,session_storage,n2o_session).
token(A)      -> (storage()):update(A), {'Token',n2o:pickle(A)}.
token(A,P)    -> (storage()):update(A), {'Token',P}.
ttl()         -> application:get_env(n2o,ttl,60*15).
till(Now,TTL) -> from(to(Now)+TTL).
prolongate()  -> application:get_env(n2o,nitro_prolongate,false).
sid(Seed)     -> n2o_secret:sid(Seed).

% API

authenticate([], Pickle) ->
    case n2o:depickle(Pickle) of
        <<>> -> token(auth(sid(os:timestamp()),expire()));
        {{Sid,'auth'},{Till,[]}} = Auth ->
            case {expired(Till), prolongate()} of
                 {false,false} -> token(Auth,Pickle);
                  {false,true} -> move(Sid), token(auth(Sid,expire()));
                      {true,_} -> (storage()):delete({Sid,auth}),
                                  token(auth(sid(os:timestamp()),expire()))
            end
    end.

get_value(Session, Key, Default) ->
    case (storage()):lookup({Session,Key}) of
         [] -> Default;
         {{Session,Key},{Till,Value}} -> case expired(Till) of
                false -> Value;
                true -> (storage()):delete({Session,Key}), Default end end.

set_value(Session, Key, Value) ->
    (storage()):update({{Session,Key},{expire(),Value}}), Value.

move(Sid) ->
    [ (storage()):update({{Sid,Key},{expire(),Val}}) || {{_,Key},{_,Val}} <- ets:select(cookies,
        ets:fun2ms(fun(A) when (element(1,element(1,A)) == Sid) -> A end)) ], ok.

clear(Session) ->
    [ ets:delete(cookies,X) || X <- ets:select(cookies,
        ets:fun2ms(fun(A) when (element(1,element(1,A)) == Session) -> element(1,A) end)) ], ok.

lookup({Session,Key}) ->
    case ets:lookup(cookies,{Session,Key}) of [Value] -> Value; Values -> Values end.

update(Token) ->
    ets:insert(cookies,Token).

delete({Session,Key}) ->
    ets:delete(cookies,{Session,Key}).

invalidate_sessions() ->
    ets:foldl(fun(X,A) -> {Sid,Key} = element(1,X),
    get_value(Sid,Key,[]), A end, 0, cookies),ok.

% TESTS

positive_test() ->
    application:set_env(n2o,nitro_prolongate,false),
    {'Token',B}=n2o_session:authenticate("",""),
    {{SID,Key},{Till,[]}} = n2o:depickle(B),
    {'Token',C}=n2o_session:authenticate("",B),
    {{SID,Key},{Till,[]}} = n2o:depickle(C),
    delete({SID,'auth'}),
    true=(C==B).

negative_test() ->
    application:set_env(n2o,nitro_prolongate,false),
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
    delete({SID0,auth}),
    delete({SID1,auth}),
    delete({SID2,auth}),
    TokenWasChanged == NewTokenIsValid.

test_set_get_value() ->
    InputValue = base64:encode(crypto:strong_rand_bytes(8)),
    SID = sid(os:timestamp()),
    Key = base64:encode(crypto:strong_rand_bytes(8)),
    set_value(SID, Key, InputValue),
    ResultValue = get_value(SID, Key, []),
    delete({SID,Key}),
    InputValue == ResultValue.
