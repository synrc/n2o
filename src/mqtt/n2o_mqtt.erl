-module(n2o_mqtt).
-export([proc/2,send/3]).
-include_lib("n2o/include/n2o.hrl").

send(C,T,M) ->
    emqtt:publish(C,T,M).

proc(init,#pi{name=Name}=Async) ->
    process_flag(trap_exit, true),

    [_,Srv,Node|_] = string:tokens(Name, "/"),

    Opt = proplists:get_value(list_to_atom(Srv), application:get_env(n2o, mqtt_services, []), []),
    Own = proplists:get_value(owner, Opt, none),
    Ps  = proplists:get_value(protocols, Opt, []),
    QoS = proplists:get_value(qos, Opt, 2),

    Topic = {?EV_TOPIC(Own,Srv,Node),QoS},

    case emqtt:start_link(#{owner => self(),
                            client_id => Name,
                            host => application:get_env(n2o, mqtt_host, {127,0,0,1}),
                            port => application:get_env(n2o, mqtt_tcp_port, 1883)}) of
        {ok, Conn} ->
            case emqtt:connect(Conn) of
                {ok,_} -> case emqtt:subscribe(Conn, Topic) of {ok,_,_} ->
                               {ok,Async#pi{state=#mqcn{conn=Conn, proto=Ps}}};
                               {error, Error} -> {error, Error} end;
                {error, Error} -> {error, Error} end;
        ignore -> ignore;
        {error, Error} -> {error, Error}
    end;

proc({'EXIT', _Pid, {shutdown, Reason}}, State) ->
    io:format("MQTT disconnected ~p.~n State: ~p~n", [Reason, State]),
    % transient pi will be restarted if terminated abnormally
    {stop, tcp_closed, State};
proc({disconnected, shutdown, tcp_closed}, State) ->
    io:format("MQTT disconnected ~p~n", [State]),
    {stop, tcp_closed, State};

proc({ring, Srv, {publish, #{topic:=T} = Request}}, State) ->
    io:format("MQTT Ring message ~p. App:~p~n.", [T, Srv]),

    [Ch,Own,_,P,Node,Vsn|Ci] = string:tokens(binary_to_list(T), "/"),
    Cid = case Ci of [] -> ""; [Id|_] -> Id end,
    T2 = lists:join("/", ["",Ch,Own,atom_to_list(Srv),P,Node,Vsn,Cid]),

    proc({publish, Request#{topic := iolist_to_binary(T2)}}, State);

proc({publish, _}, State=#pi{state=#mqcn{proto=[]}}) ->
    {stop, not_handled, State};

proc({publish, #{payload := Request, topic := Topic}}, State=#pi{state=#mqcn{conn=C,proto=Ps}}) ->
    [_,_Own,_Srv,P,Node,_Vsn|Ci] = string:tokens(binary_to_list(Topic), "/"),

    From = case Ci of [] -> ?ACT_TOPIC(P); [Cid|_] -> ?ACT_TOPIC(P,Cid) end,

    Ctx=#cx{module=list_to_atom(P),node=Node,params=Ci,client_pid=C,from=From},
    put(context,Ctx),

    % handle_info may initiate the proc
    % so valid response are {noreply,_,_} variations and {stop,_,_}
    Req = try n2o:decode(Request) catch error:badarg -> {error, badarg} end,

    case n2o_proto:try_info(Req,[],Ctx,Ps) of
        {reply,{_,      <<>>},_,_}           -> {noreply, State};
        {reply,{bert,   Term},_,#cx{from=X}} -> send(C,X,n2o_bert:encode(Term)), {noreply, State};
        {reply,{json,   Term},_,#cx{from=X}} -> send(C,X,n2o_json:encode(Term)), {noreply,State};
        {reply,{text,   Term},_,#cx{from=X}} -> send(C,X,Term), {noreply, State};
        {reply,{binary, Term},_,#cx{from=X}} -> send(C,X,Term), {noreply,State};
        {reply,{default,Term},_,#cx{from=X}} -> send(C,X,n2o:encode(Term)), {noreply,State};
        {reply,{Encoder,Term},_,#cx{from=X}} -> send(C,X,Encoder:encode(Term)), {noreply, State};
                                       Reply -> ?LOG_ERROR({"Invalid Return", Reply}), {noreply, State}
    end;

proc(Unknown,Async=#pi{name=Name}) ->
    io:format("MQTTv5 [~s]: ~p~n",[Name,Unknown]),
    {noreply, Async}.
