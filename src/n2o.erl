-module(n2o).
-description('N2O DAS MQTT TCP WebSocket').
-behaviour(supervisor).
-behaviour(application).
-include("n2o.hrl").
-export([start/2, stop/1, init/1, proc/2, version/0, ring/0, to_binary/1, bench/0]).

% SERVICES

-export([send/2,reg/1,unreg/1,reg/2]).        % mq
-export([pickle/1,depickle/1]).               % pickle
-export([encode/1,decode/1]).                 % format
-export([info/3,warning/3,error/3]).          % log
-export([session/1,session/2,user/1,user/0]). % session

% BUILT-IN BACKENDS

-export([cache/2,cache/3,cache/4,invalidate_cache/1]).               % cache
-export([erroring/0,stack/2,erroring/2,stack_trace/2,error_page/2]). % error

% START VNODE HASH RING

stop(_)    -> catch n2o_vnode:unload(), ok.
start(_,_) -> catch n2o_vnode:load([]), X = supervisor:start_link({local,n2o},n2o, []),
              n2o_async:start(#handler{module=?MODULE,class=caching,group=n2o,state=[],name="timer"}),
              [ n2o_async:start(#handler{module=n2o_vnode,class=ring,group=n2o,state=[],name=Pos})
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

% ERROR

erroring() -> application:get_env(n2o,erroring,n2o).
stack(Error, Reason) -> (erroring()):stack_trace(Error, Reason).
erroring(Class, Error) -> (erroring()):error_page(Class, Error).

% SESSION

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

% ERROR

stack_trace(Error, Reason) ->
    Stacktrace = [case A of
         { Module,Function,Arity,Location} ->
             { Module,Function,Arity,proplists:get_value(line, Location) };
         Else -> Else end
    || A <- erlang:get_stacktrace()],
    [Error, Reason, Stacktrace].

error_page(Class,Error) ->
    io_lib:format("ERROR:  ~w:~w\r~n\r~n",[Class,Error]) ++
    "STACK: " ++
    [ io_lib:format("\t~w:~w/~w:~w\n",
        [ Module,Function,Arity,proplists:get_value(line, Location) ])
    ||  { Module,Function,Arity,Location} <- erlang:get_stacktrace() ].


% TIMER

proc(init,#handler{}=Async) ->
    n2o:info(?MODULE,"Proc Init: ~p\r~n",[init]),
    Timer = timer_restart(ping()),
    {ok,Async#handler{state=Timer}};

proc({timer,ping},#handler{state=Timer}=Async) ->
    erlang:cancel_timer(Timer),
    n2o:info(?MODULE,"n2o Timer: ~p\r~n",[ping]),
    n2o:invalidate_cache(caching),
    (n2o_session:storage()):invalidate_sessions(),
    {reply,ok,Async#handler{state=timer_restart(ping())}}.

invalidate_cache(Table) -> ets:foldl(fun(X,_) -> n2o:cache(Table,element(1,X)) end, 0, Table).
timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(1000*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(n2o,timer,{0,1,0}).

% LOG

logger()       -> application:get_env(?MODULE,logger,n2o_io).
log_modules()  -> application:get_env(?MODULE,log_modules,[]).
log_level()    -> application:get_env(?MODULE,log_level,info).

level(none)    -> 3;
level(error)   -> 2;
level(warning) -> 1;
level(_)       -> 0.

log(M,F,A,Fun) ->
    case level(Fun) < level(log_level()) of
         true  -> skip;
         false -> case    log_modules() of
             any       -> (logger()):Fun(M,F,A);
             []        -> skip;
             Allowed   -> case lists:member(M, Allowed) of
                 true  -> (logger()):Fun(M,F,A);
                 false -> skip end end end.

info   (Module, String, Args) -> log(Module,  String, Args, info).
warning(Module, String, Args) -> log(Module,  String, Args, warning).
error  (Module, String, Args) -> log(Module,  String, Args, error).

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
