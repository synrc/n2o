-module(n2o).
-compile(export_all).
-description('N2O DAS MQTT TCP WebSocket').
-behaviour(supervisor).
-behaviour(application).
-include("n2o.hrl").
-include("n2o_core.hrl").
-include("n2o_api.hrl").
-export([start/2, stop/1, init/1, proc/2, version/0, ring/0, to_binary/1, bench/0]).

% SERVICES

-export([send/2,reg/1,unreg/1,reg/2]).        % mq
-export([pickle/1,depickle/1]).               % pickle
-export([encode/1,decode/1]).                 % format
-export([session/1,session/2,user/1,user/0]). % session
-export([cache/2,cache/3,cache/4,invalidate_cache/1]). % cache

% START VNODE HASH RING

stop(_)    -> catch n2o_vnode:unload(), ok.
start(_,_) -> catch n2o_vnode:load([]), X = supervisor:start_link({local,n2o},n2o, []),
              n2o_pi:start(#pi{module=?MODULE,table=caching,sup=n2o,state=[],name="timer"}),
              [ n2o_pi:start(#pi{module=n2o_vnode,table=ring,sup=n2o,state=[],name=Pos})
                || {{_,_},Pos} <- lists:zip(ring(),lists:seq(1,length(ring()))) ],
                X.

ring()         -> array:to_list(n2o_ring:ring()).
rand_vnode()   -> rand:uniform(length(ring())).
opt()          -> [ set, named_table, { keypos, 1 }, public ].
tables()       -> application:get_env(n2o,tables,[ cookies, file, caching, ring, async ]).
storage_init() -> [ ets:new(X,opt()) || X <- tables() ].
init([])       -> storage_init(),
                  n2o_ring:init([{node(),1,4}]),
                  { ok, { { one_for_one, 1000, 10 }, [] } }.

% MQTT vs OTP benchmarks

bench() -> [bench_mqtt(),bench_otp()].
run()   -> 10000.

bench_mqtt() -> N = run(), {T,_} = timer:tc(fun() -> [ begin Y = lists:concat([X rem 16]),
    n2o_vnode:send_reply(<<"clientId">>,n2o:to_binary(["events/1/",Y]),term_to_binary(X))
                               end || X <- lists:seq(1,N) ], ok end),
           {mqtt,trunc(N*1000000/T),"msgs/s"}.

bench_otp() -> N = run(), {T,_} = timer:tc(fun() ->
     [ n2o_ring:send({publish, n2o:to_binary(["events/1/",
              lists:concat([(X rem length(n2o:ring())) + 1]),"/index/anon/room/"]),
                      term_to_binary(X)}) || X <- lists:seq(1,N) ], ok end),
     {otp,trunc(N*1000000/T),"msgs/s"}.

% MQ

mq()      -> application:get_env(n2o,mq,n2o_gproc).
send(X,Y) -> (mq()):send(X,Y).
reg(X)    -> (mq()):reg(X).
unreg(X)  -> (mq()):unreg(X).
reg(X,Y)  -> (mq()):reg(X,Y).

% PICKLE

pickler() -> application:get_env(n2o,pickler,n2o_secret).
pickle(Data) -> (pickler()):pickle(Data).
depickle(SerializedData) -> (pickler()):depickle(SerializedData).

% SESSION

sid() -> #cx{session=SID}=get(context), SID.
session() -> application:get_env(n2o,session,n2o_session).
session(Key)        -> #cx{session=SID}=get(context), (session()):get_value(SID, Key, []).
session(Key, Value) -> #cx{session=SID}=get(context), (session()):set_value(SID, Key, Value).
user()              -> case session(user) of undefined -> []; E -> lists:concat([E]) end.
user(User)          -> session(user,User).

% FORMAT

formatter() -> application:get_env(n2o,formatter,n2o_bert).
encode(Term) -> (formatter()):encode(Term).
decode(Term) -> (formatter()):decode(Term).

% CACHE

cache(Tab, Key, Value, Till) -> ets:insert(Tab,{Key,Till,Value}), Value.
cache(Tab, Key, undefined)   -> ets:delete(Tab,Key);
cache(Tab, Key, Value)       -> ets:insert(Tab,{Key,n2o_session:till(calendar:local_time(),
                                                    n2o_session:ttl()),Value}), Value.
cache(Tab, Key) ->
    Res = ets:lookup(Tab,Key),
    Val = case Res of [] -> []; [Value] -> Value; Values -> Values end,
    case Val of [] -> [];
                {_,infinity,X} -> X;
                {_,Expire,X} -> case Expire < calendar:local_time() of
                                  true ->  ets:delete(Tab,Key), [];
                                  false -> X end end.

% TIMER

proc(init,#pi{}=Async) ->
    Timer = timer_restart(ping()),
    {ok,Async#pi{state=Timer}};

proc({timer,ping},#pi{state=Timer}=Async) ->
    erlang:cancel_timer(Timer),
    n2o:invalidate_cache(caching),
    (n2o_session:storage()):invalidate_sessions(),
    {reply,ok,Async#pi{state=timer_restart(ping())}}.

invalidate_cache(Table) -> ets:foldl(fun(X,_) -> n2o:cache(Table,element(1,X)) end, 0, Table).
timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(1000*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(n2o,timer,{0,1,0}).

%%

to_binary(A) when is_atom(A) -> atom_to_binary(A,latin1);
to_binary(B) when is_binary(B) -> B;
to_binary(T) when is_tuple(T) -> term_to_binary(T);
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> float_to_binary(F,[{decimals,9},compact]);
to_binary(L) when is_list(L) ->  iolist_to_binary(L).

%

version() -> proplists:get_value(vsn,
             element(2,application:get_all_key(n2o))).

% END
