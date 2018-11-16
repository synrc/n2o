-module(n2o).
-description('N2O DAS MQTT TCP WebSocket').
-behaviour(supervisor).
-behaviour(application).
-include("n2o.hrl").
-include("emqttd.hrl").
-compile(export_all).
-export([start/2, stop/1, init/1, proc/2]).

%% N2O-MQTT Topic Format
%%
%% Client: 1. actions/:vsn/:module/:client
%% Server: 2. events/:vsn/:node/:module/:user/:client/:token

load(Env) ->
    emqttd:hook('client.connected',    fun ?MODULE:on_client_connected/3,     [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3,  [Env]),
    emqttd:hook('client.subscribe',    fun ?MODULE:on_client_subscribe/4,     [Env]),
    emqttd:hook('client.unsubscribe',  fun ?MODULE:on_client_unsubscribe/4,   [Env]),
    emqttd:hook('session.created',     fun ?MODULE:on_session_created/3,      [Env]),
    emqttd:hook('session.subscribed',  fun ?MODULE:on_session_subscribed/4,   [Env]),
    emqttd:hook('session.unsubscribed',fun ?MODULE:on_session_unsubscribed/4, [Env]),
    emqttd:hook('session.terminated',  fun ?MODULE:on_session_terminated/4,   [Env]),
    emqttd:hook('message.publish',     fun ?MODULE:on_message_publish/2,      [Env]),
    emqttd:hook('message.delivered',   fun ?MODULE:on_message_delivered/4,    [Env]),
    emqttd:hook('message.acked',       fun ?MODULE:on_message_acked/4,        [Env]).

stop(_)    -> unload(), ok.
start(_,_) -> catch load([]), X = supervisor:start_link({local,n2o},n2o, []),
              n2o_async:start(#handler{module=?MODULE,class=caching,group=n2o,state=[],name="timer"}),
              [ n2o_async:start(#handler{module=n2o_vnode,class=ring,group=n2o,state=[],name=Pos})
                || {{_,_},Pos} <- lists:zip(ring(),lists:seq(1,length(ring()))) ],
                X.
ring()     -> n2o_ring:ring_list().
ring_max() -> length(ring()).
rand_vnode() -> rand:uniform(ring_max()).

init([])   -> storage_init(),
              n2o_ring:init([{node(),1,4}]),
              { ok, { { one_for_one, 1000, 10 }, [] } }.

% MQTT vs OTP benchmarks

bench() -> [bench_mqtt(),bench_otp()].
run()   -> 10000.

bench_mqtt() -> N = run(), {T,_} = timer:tc(fun() -> [ begin Y = lists:concat([X rem 16]),
    n2o:send_reply(<<"clientId">>,n2o:to_binary(["events/1/",Y]),term_to_binary(X))
                               end || X <- lists:seq(1,N) ], ok end),
           {mqtt,trunc(N*1000000/T),"msgs/s"}.

bench_otp() -> N = run(), {T,_} = timer:tc(fun() ->
     [ n2o_ring:send({publish, n2o:to_binary(["events/1/",
              lists:concat([(X rem length(n2o:ring())) + 1]),"/index/anon/room/"]),
                      term_to_binary(X)}) || X <- lists:seq(1,N) ], ok end),
     {otp,trunc(N*1000000/T),"msgs/s"}.

on_client_connected(_ConnAck, Client=#mqtt_client{client_id= <<"emqttc",_/bytes>>}, _) -> {ok, Client};
on_client_connected(_ConnAck, Client = #mqtt_client{}, _Env) -> {ok, Client}.
on_client_disconnected(_Reason, _Client = #mqtt_client{}, _Env) -> ok.
on_client_subscribe(_ClientId, _Username, TopicTable, _Env) -> {ok, TopicTable}.
on_client_unsubscribe(_ClientId, _Username, TopicTable, _Env) -> {ok, TopicTable}.
on_session_created(_ClientId, _Username, _Env) -> ok.
on_session_subscribed(<<"emqttd",_/binary>>,_,{<<"actions/",_, "/",_/binary>> =Topic,Opts},_) -> {ok,{Topic,Opts}};
on_session_subscribed(_ClientId, _Username, {Topic, Opts}, _Env) -> {ok, {Topic,Opts}}.
on_session_unsubscribed(_ClientId, _Username, {_Topic, _Opts}, _Env) -> ok.
on_session_terminated(_ClientId, _Username, _Reason, _Env) -> ok.
on_message_delivered(_ClientId, _Username, Message, _Env) -> {ok,Message}.
on_message_acked(_ClientId, _Username, Message, _Env) -> {ok,Message}.
on_message_publish(Message = #mqtt_message{topic = <<"actions/", _/binary>>, from=_From}, _Env) -> {ok, Message};
on_message_publish(#mqtt_message{topic = <<"events/", _TopicTail/binary>> = Topic, qos=Qos,
    from={ClientId,_},payload = Payload}=Message, _Env) ->
    {Module, ValidateFun} = application:get_env(?MODULE, validate, {?MODULE, validate}),
    Res = case emqttd_topic:words(Topic) of
        [E,V,'',M,U,_C,T] -> {Mod,F} = application:get_env(?MODULE, vnode, {?MODULE, get_vnode}),
            NewTopic = emqttd_topic:join([E,V,Mod:F(ClientId,Payload),M,U,ClientId,T]),
            emqttd:publish(emqttd_message:make(ClientId, Qos, NewTopic, Payload)), skip;
        %% @NOTE redirect to vnode
        [_E,_V,_N,_M,_U,ClientId,_T] ->
            case Module:ValidateFun(Payload) of ok -> {ok, Message}; _ -> skip end;
        [E,V,N,M,U,_C,T] -> NewTopic = emqttd_topic:join([E,V,N,M,U,ClientId,T]),
            emqttd:publish(emqttd_message:make(ClientId, Qos, NewTopic, Payload)), skip;
        %% @NOTE redirects to event topic with correct ClientId
        _ -> case Module:ValidateFun(Payload) of ok -> {ok, Message}; _ -> skip end
    end,
    case Res of
       {ok, _} -> case Module:ValidateFun(Payload) of ok -> Res; _ -> skip end;
        _ -> Res
    end;
on_message_publish(Message, _) -> {ok,Message}.

unload() ->
    emqttd:unhook('client.connected',     fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected',  fun ?MODULE:on_client_disconnected/3),
    emqttd:unhook('client.subscribe',     fun ?MODULE:on_client_subscribe/4),
    emqttd:unhook('client.unsubscribe',   fun ?MODULE:on_client_unsubscribe/4),
    emqttd:unhook('session.subscribed',   fun ?MODULE:on_session_subscribed/4),
    emqttd:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
    emqttd:unhook('message.publish',      fun ?MODULE:on_message_publish/2),
    emqttd:unhook('message.delivered',    fun ?MODULE:on_message_delivered/4),
    emqttd:unhook('message.acked',        fun ?MODULE:on_message_acked/4).

send_reply(ClientId, Topic, Message) -> send_reply(ClientId, 0, Topic, Message).
send_reply(ClientId, QoS, Topic, Message) ->
    emqttd:publish(emqttd_message:make(ClientId, QoS, Topic, Message)).

% Pickling n2o:send/1

mq()      -> application:get_env(n2o,mq,n2o_gproc).
send(X,Y) -> (mq()):send(X,Y).
reg(X)    -> (mq()):reg(X).
unreg(X)  -> (mq()):unreg(X).
reg(X,Y)  -> (mq()):reg(X,Y).

% Pickling n2o:pickle/1

pickler() -> application:get_env(n2o,pickler,n2o_secret).
pickle(Data) -> (pickler()):pickle(Data).
depickle(SerializedData) -> (pickler()):depickle(SerializedData).

% Error handler n2o:error/2 n2o:stack/2

erroring() -> application:get_env(n2o,erroring,n2o).
stack(Error, Reason) -> (erroring()):stack_trace(Error, Reason).
erroring(Class, Error) -> (erroring()):error_page(Class, Error).

% Formatter

formatter() -> application:get_env(n2o,formatter,n2o_bert).
encode(Term) -> (formatter()):encode(Term).
decode(Term) -> (formatter()):decode(Term).

% Cache facilities n2o:cache/[1,2,3]

proc(init,#handler{}=Async) ->
    n2o:info(?MODULE,"Proc Init: ~p\r~n",[init]),
    Timer = timer_restart(ping()),
    {ok,Async#handler{state=Timer}};

proc({timer,ping},#handler{state=Timer}=Async) ->
    erlang:cancel_timer(Timer),
    n2o:info(?MODULE,"n2o Timer: ~p\r~n",[ping]),
    n2o:invalidate_cache(caching),
    {reply,ok,Async#handler{state=timer_restart(ping())}}.

invalidate_cache(Table) -> ets:foldl(fun(X,_) -> n2o:cache(Table,element(1,X)) end, 0, Table).
timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(1000*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(n2o,timer,{0,1,0}).
ttl() -> application:get_env(n2o,ttl,60*1).
till(Now,TTL) -> calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now) + TTL), infinity.

opt() -> [ set, named_table, { keypos, 1 }, public ].
tables() -> application:get_env(n2o,tables,[ cookies, file, caching, ring, async ]).
storage_init()   -> [ ets:new(X,opt()) || X <- tables() ].
cache(Tab, Key, undefined)   -> ets:delete(Tab,Key);
cache(Tab, Key, Value)       -> ets:insert(Tab,{Key,till(calendar:local_time(), ttl()),Value}), Value.
cache(Tab, Key, Value, Till) -> ets:insert(Tab,{Key,Till,Value}), Value.
cache(Tab, Key) ->
    Res = ets:lookup(Tab,Key),
    Val = case Res of [] -> []; [Value] -> Value; Values -> Values end,
    case Val of [] -> [];
                {_,infinity,X} -> X;
                {_,Expire,X} -> case Expire < calendar:local_time() of
                                  true ->  ets:delete(Tab,Key), [];
                                  false -> X end end.

% Context Variables and URL Query Strings from ?REQ and ?CTX n2o:q/1 n2o:qc/[1,2]

q(Key) -> Val = get(Key), case Val of undefined -> qc(Key); A -> A end.
qc(Key) -> CX = get(context), qc(Key,CX).
qc(Key,Ctx) -> proplists:get_value(n2o:to_binary([Key]),Ctx#cx.params).

atom(List) when is_list(List) -> list_to_atom(string:join([ lists:concat([L]) || L <- List],"_"));
atom(Scalar) -> list_to_atom(lists:concat([Scalar])).

% These api are not really API

temp_id() -> "auto" ++ integer_to_list(erlang:unique_integer() rem 1000000).
append(List, Key, Value) -> case Value of undefined -> List; _A -> [{Key, Value}|List] end.
render(X) -> wf_render:render(X).

version() -> proplists:get_value(vsn,element(2,application:get_all_key(n2o))).

keyset(Name,Pos,List,New) ->
    case lists:keyfind(Name,Pos,List) of
        false -> [New|List];
        _Element -> lists:keyreplace(Name,Pos,List,New) end.

actions() -> get(actions).
actions(Ac) -> put(actions,Ac).

clear_actions() -> put(actions,[]).
add_action(Action) ->
    Actions = case get(actions) of undefined -> []; E -> E end,
    put(actions,Actions++[Action]).

fold(Fun,Handlers,Ctx) ->
    lists:foldl(fun({_,Module},Ctx1) ->
        {ok,_,NewCtx} = Module:Fun([],Ctx1),
        NewCtx end,Ctx,Handlers).

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

session() -> application:get_env(n2o,session,n2o_session).
session(Key)        -> #cx{session=SID}=get(context), (session()):get_value(SID, Key, []).
session(Key, Value) -> #cx{session=SID}=get(context), (session()):set_value(SID, Key, Value).
user()              -> case session(user) of undefined -> []; E -> lists:concat([E]) end.
user(User)          -> session(user,User).

subscribe(X,Y) -> subscribe(X,Y,[{qos,2}]).
subscribe(X,Y,[{qos,Z}]) -> subscribe_cli(X,[{Y,Z}]).

unsubscribe(X,Y) -> unsubscribe(X,Y,[{qos,2}]).
unsubscribe(X,Y,[{qos,Z}]) -> unsubscribe_cli(X,[{Y,Z}]).

subscribe_cli(ClientId, TopicTable) ->
    [ begin
        kvs:put({mqtt_subscription, ClientId, Topic}),
        kvs:put({mqtt_subproperty, {Topic, ClientId}, [{qos,Qos}]}),
        emqttd_pubsub:add_subscriber(Topic,ClientId,[{qos,Qos}])
      end || {Topic,Qos} <- TopicTable ].

unsubscribe_cli(ClientId, TopicTable)->
    DelFun = fun() -> [ begin
             mnesia:delete_object({mqtt_subscription, ClientId, Topic}),
             kvs:delete(mqtt_subproperty, {Topic, ClientId}),
             mnesia:delete_object({mqtt_subscriber, Topic, ClientId})
            end || {Topic,_Qos} <- TopicTable ] end,
    case mnesia:is_transaction() of
        true  -> DelFun();
        false -> mnesia:transaction(DelFun)
    end,
    [(not ets:member(mqtt_subscriber, Topic)) andalso
      emqttd_router:del_route(Topic) || {Topic,_Qos} <- TopicTable ].

get_vnode(ClientId) -> get_vnode(ClientId, []).
get_vnode(ClientId, _) ->
    [H|_] = binary_to_list(erlang:md5(ClientId)),
    integer_to_binary(H rem ring_max() + 1).

validate(_Payload) -> ok.

% Tiny Logging Framework

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

