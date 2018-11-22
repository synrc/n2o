-module(n2o_vnode).
-description('N2O MQTT Backend').
-include("n2o.hrl").
-include("emqttd.hrl").
-export([proc/2]).
-export([get_vnode/1,get_vnode/2,validate/1,send_reply/3,send_reply/4,send/3,send/4]).
-export([subscribe/3,subscribe/2,unsubscribe/3,unsubscribe/2,subscribe_cli/2,unsubscribe_cli/2]).
-export([load/1,unload/0]).
-export([on_client_connected/3, on_client_disconnected/3, on_client_subscribe/4,
         on_client_unsubscribe/4, on_session_created/3, on_session_subscribed/4,
         on_session_unsubscribed/4, on_session_terminated/4, on_message_publish/2,
         on_message_delivered/4, on_message_acked/4 ]).

%% N2O-MQTT Topic Format

%% Client: 1. actions/:vsn/:module/:client
%% Server: 2. events/:vsn/:node/:module/:user/:client/:token

%% server and client sends

send_reply(ClientId, Topic, Message) -> send_reply(ClientId, 0, Topic, Message).
send_reply(ClientId, QoS, Topic, Message) ->
    emqttd:publish(emqttd_message:make(ClientId, QoS, Topic, Message)).

send(C,T,M)      -> send(C, T, M, [{qos,2}]).
send(C,T,M,Opts) -> emqttc:publish(C, T, M, Opts).

% N2O VNODE SERVER for MQTT

debug(Name,Topic,BERT,Address,Return) ->
    case application:get_env(n2o,dump_loop,no) of
         yes -> n2o:info(?MODULE,"VNODE:~p Message on topic ~tp.\r~n", [Name, Topic]),
                n2o:info(?MODULE,"BERT: ~tp\r~nAddress: ~p\r~n",[BERT,Address]),
                n2o:info(?MODULE,"on_message_publish: ~s.\r~n", [Topic]),
                case Return of
                     {error,R} -> n2o:info(?MODULE,"ERROR: ~p~n",[R]);
                             _ -> skip
                end,
                ok;
           _ -> skip end.


sid() -> application:get_env(n2o,token_as_sid,false).
fix('')          -> index;
fix(<<"index">>) -> index;
fix(Module)      -> list_to_atom(binary_to_list(Module)).

gen_name(Pos) when is_integer(Pos) -> gen_name(integer_to_list(Pos));
gen_name(Pos) -> n2o:to_binary([lists:flatten([io_lib:format("~2.16.0b",[X])
              || <<X:8>> <= list_to_binary(atom_to_list(node())++"_"++Pos)])]).

proc(init,#handler{name=Name}=Async) ->
    n2o:info(?MODULE,"VNode Init: ~p\r~n",[Name]),
    case catch emqttc:module_info() of
         {'EXIT',_} -> {ok,Async#handler{state=[],seq=0}};
         _ -> {ok, C} = emqttc:start_link([{host, "127.0.0.1"},
                            {client_id, gen_name(Name)},
                            {clean_sess, false},
                            {logger, {console, error}},
                            {reconnect, 5}]),
                  {ok,Async#handler{state=C,seq=0}} end;

proc({publish,_,_}, State=#handler{state=[]}) -> {reply,[],State};
proc({publish, To, Request},
    State  = #handler{name=Name,state=C,seq=S}) ->
    Addr   = emqttd_topic:words(To),
    Bert   = n2o:decode(Request),
    Return = case Addr of
        [ _Origin, Vsn, Node, Module, _Username, Id, Token | _ ] ->
        From = n2o:to_binary(["actions/", Vsn, "/", Module, "/", Id]),
        Sid  = case sid() of
                    true -> n2o:to_binary(Token);
                    false -> case n2o:depickle(n2o:to_binary(Token)) of
                             {{A,_},_} -> A; B -> B end
               end,
        Ctx  = #cx { module=fix(Module), session=Sid, node=Node,
                     params=Id, client_pid=C, from = From, vsn = Vsn},
        put(context, Ctx),
        try case n2o_proto:info(Bert,[],Ctx) of
                 {reply,{_,      <<>>},_,_}           -> skip;
                 {reply,{bert,   Term},_,#cx{from=X}} -> {ok,send(C,X,n2o_bert:encode(Term))};
                 {reply,{json,   Term},_,#cx{from=X}} -> {ok,send(C,X,n2o_json:encode(Term))};
                 {reply,{binary, Term},_,#cx{from=X}} -> {ok,send(C,X,Term)};
                 {reply,{default,Term},_,#cx{from=X}} -> {ok,send(C,X,n2o:encode(Term))};
                 {reply,{Encoder,Term},_,#cx{from=X}} -> {ok,send(C,X,Encoder:encode(Term))};
                                                Reply -> {error,{"Invalid Return",Reply}}
            end
        catch Err:Rea ->
            n2o:error(?MODULE,"Catch:~p~n",[n2o:stack_trace(Err,Rea)])
        end;
        Addr -> {error,{"Unknown Address",Addr}} end,
    debug(Name,To,Bert,Addr,Return),
    {reply, Return, State#handler{seq=S+1}};

proc({mqttc, C, connected}, State=#handler{name=Name,state=C,seq=S}) ->
    emqttc:subscribe(C, n2o:to_binary([<<"events/+/">>, lists:concat([Name]),"/#"]), 2),
    {ok, State#handler{seq = S+1}};

proc(Unknown,#handler{seq=S}=Async) ->
    {reply,{uknown,Unknown,S},Async#handler{seq=S+1}}.

% MQTT HELPERS

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

% MQTT HOOKS

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

get_vnode(ClientId) -> get_vnode(ClientId, []).
get_vnode(ClientId, _) ->
    [H|_] = binary_to_list(erlang:md5(ClientId)),
    integer_to_binary(H rem (length(n2o:ring())) + 1).

validate(_Payload) -> ok.
