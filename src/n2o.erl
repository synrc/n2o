-module(n2o).

-description('N2O Application Server').

-behaviour(supervisor).

-behaviour(application).

-include_lib("n2o/include/n2o.hrl").

-include_lib("n2o/include/n2o_core.hrl").

-include_lib("n2o/include/n2o_api.hrl").

-export([start/2,
         stop/1,
         init/1,
         proc/2,
         version/0,
         to_binary/1,
         bench/0]).

% SERVICES

-export([send/2, reg/1, unreg/1, reg/2]).

-export([pickle/1, depickle/1]).

-export([encode/1, decode/1]).

-export([session/1, session/2, user/1, user/0]).

-export([start_mqtt/0, start_ws/0, start_tcp/0, sid/0]).

-export([cache/2,
         cache/3,
         cache/4,
         invalidate_cache/1]).

% START VNODE HASH RING

init([]) ->
    storage_init(),
    mq_init(),
    {ok, {{one_for_one, 1000, 10}, []}}.

stop(_) -> ok.

bench() -> ok.

start(_, _) ->
    S = supervisor:start_link({local, n2o}, n2o, []),
    start_timer(),
    Space = 4,                 % ring partitions
    Default = [erp],           % applications
    Protocols = [mqtt, ws, tcp], % protocols
    % Generic loop for applications, its rings and exposed protocols
    [begin
         lists:flatmap(fun Rng({B, _}) -> Rng(B);
                           Rng(B) ->
                               Key = fun (I) -> lists:concat(["/", P, "/", B, "/", I])
                                     end,
                               X = lists:map(fun (I) -> Key(I) end,
                                             lists:seq(1, Space)),
                               application:set_env(B,
                                                   n2o_ring:tab2ring(P),
                                                   n2o_ring:new(Space, X)),
                               X
                       end,
                       application:get_env(n2o, n2o_ring:tab2srv(P), Default))
     end
     || P <- Protocols],
    S.

start_mqtt() ->
    lists:map(fun start_mqtt/1, n2o_ring:members(mqtt)).

start_ws() ->
    lists:map(fun start_ws/1, n2o_ring:members(ws)).

start_tcp() ->
    lists:map(fun start_tcp/1, n2o_ring:members(tcp)).

start_mqtt(Node) ->
    n2o_pi:start(#pi{module = n2o_mqtt, table = mqtt,
                     sup = n2o, state = [], name = Node}).

start_ws(Node) ->
    n2o_pi:start(#pi{module = n2o_ws, table = ws, sup = n2o,
                     state = [], name = Node}).

start_tcp(Node) ->
    n2o_pi:start(#pi{module = n2o_tcp, table = tcp,
                     sup = n2o, state = [], name = Node}).

start_timer() ->
    n2o_pi:start(#pi{module = n2o, table = caching,
                     sup = n2o, state = [], name = "/timer"}).

% ETS

opt() -> [set, named_table, {keypos, 1}, public].

tables() ->
    application:get_env(n2o,
                        tables,
                        [cookies, file, caching, ws, mqtt, tcp, async]).

storage_init() -> [ets:new(X, opt()) || X <- tables()].

% MQ

mq() -> application:get_env(n2o, mq, n2o_gproc).

mq_init() -> (mq()):init().

send(X, Y) -> (mq()):send(X, Y).

reg(X) -> (mq()):reg(X).

unreg(X) -> (mq()):unreg(X).

reg(X, Y) -> (mq()):reg(X, Y).

% PICKLE

pickler() ->
    application:get_env(n2o, pickler, n2o_secret).

pickle(Data) -> (pickler()):pickle(Data).

depickle(SerializedData) ->
    (pickler()):depickle(SerializedData).

% SESSION

sid() ->
    #cx{session = SID} = get(context),
    SID.

session() ->
    application:get_env(n2o, session, n2o_session).

session(Key) ->
    Context = get(context),
    case Context of
        #cx{session = SID} ->
            (session()):get_value(SID, Key, []);
        _ -> []
    end.

session(Key, Value) ->
    #cx{session = SID} = get(context),
    (session()):set_value(SID, Key, Value).

user() ->
    case session(user) of
        undefined -> [];
        E -> E
    end.

user(User) -> session(user, User).

% FORMAT

formatter() ->
    application:get_env(n2o, formatter, n2o_bert).

encode(Term) -> (formatter()):encode(Term).

decode(Term) -> (formatter()):decode(Term).

% CACHE

cache(Tab, Key, Value, Till) ->
    ets:insert(Tab, {Key, {Till, Value}}),
    Value.

cache(Tab, Key, undefined) -> ets:delete(Tab, Key);
cache(Tab, Key, Value) ->
    ets:insert(Tab,
               {Key,
                {n2o_session:till(calendar:local_time(),
                                  n2o_session:ttl()),
                 Value}}),
    Value.

cache(Tab, Key) ->
    Res = ets:lookup(Tab, Key),
    Val = case Res of
              [] -> [];
              [Value] -> Value;
              Values -> Values
          end,
    case Val of
        [] -> [];
        {_, {infinity, X}} -> X;
        {_, {Expire, X}} ->
            case Expire < calendar:local_time() of
                true ->
                    ets:delete(Tab, Key),
                    [];
                false -> X
            end
    end.

% TIMER

proc(init, #pi{} = Async) ->
    Timer = timer_restart(ping()),
    {ok, Async#pi{state = Timer}};
proc({timer, ping}, #pi{state = Timer} = Async) ->
    erlang:cancel_timer(Timer),
    n2o:invalidate_cache(caching),
    (n2o_session:storage()):invalidate_sessions(),
    {noreply, Async#pi{state = timer_restart(ping())}}.

invalidate_cache(Table) ->
    ets:foldl(fun (X, _) -> n2o:cache(Table, element(1, X))
              end,
              0,
              Table).

timer_restart(Diff) ->
    {X, Y, Z} = Diff,
    erlang:send_after(1000 * (Z + 60 * Y + 60 * 60 * X),
                      self(),
                      {timer, ping}).

ping() -> application:get_env(n2o, timer, {0, 1, 0}).

%%

to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(B) when is_binary(B) -> B;
to_binary(T) when is_tuple(T) -> term_to_binary(T);
to_binary(I) when is_integer(I) ->
    to_binary(integer_to_list(I));
to_binary(F) when is_float(F) ->
    float_to_binary(F, [{decimals, 9}, compact]);
to_binary(L) when is_list(L) -> iolist_to_binary(L).

%

version() ->
    proplists:get_value(vsn,
                        element(2, application:get_all_key(n2o))).

% END
