-module(n2o_mqtt).
-export([proc/2,send/3]).
-include_lib("n2o/include/n2o.hrl").

send(C,T,M) ->
    emqtt:publish(C,T,M).

proc(init,#pi{name=Name}=Async) ->
    process_flag(trap_exit, true),
    case emqtt:start_link(#{owner => self(),
                            client_id => Name,
                            host => application:get_env(n2o, mqtt_brocker, {127,0,0,1}),
                            port => application:get_env(n2o, mqtt_port, 1883)}) of
        {ok, Conn} ->
            [_,M,Node|_] = string:tokens(Name, "/"),
            {ok,_}   = emqtt:connect(Conn),
            {ok,_,_} = emqtt:subscribe(Conn, {?SRV_TOPIC(Node,M),2}),
            {ok,Async#pi{state=Conn}};
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

proc({ring, App, {publish, #{topic:=T} = Request}}, State) ->
    io:format("MQTT Ring message ~p. App:~p~n.", [T, App]),

    [Ch,Vsn,Node,_,Usr,Cid|_] = string:tokens(binary_to_list(T), "/"),
    T2 = lists:join("/", ["",Ch,Vsn,Node,atom_to_list(App),Usr,Cid]),

    proc({publish, Request#{topic := iolist_to_binary(T2)}}, State);

proc({publish, #{payload := Request, topic := Topic}}, State=#pi{state=C}) ->
    [_Ch,_,Node,M,_Usr,Cid|_] = string:tokens(binary_to_list(Topic), "/"),
    put(context,Cx=#cx{module=list_to_atom(M),node=Node,params=Cid,client_pid=C,from=?CLI_TOPIC(M,Cid)}),

    % handle_info may initiate the proc
    % so valid response are {noreply,_,_} variations and {stop,_,_}
    Req = try n2o:decode(Request) catch error:badarg -> {error, badarg} end,

    case  n2o_proto:try_info(Req,[],Cx) of
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
    {reply,{uknown,Unknown,0},Async}.
