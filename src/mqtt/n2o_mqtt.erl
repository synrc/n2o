-module(n2o_mqtt).
-export([proc/2,send/3]).
-include_lib("n2o/include/n2o.hrl").

send(C,T,M) ->
    emqtt:publish(C,T,M).

proc(init,#pi{name=Name}=Async) ->
    case emqtt:start_link(#{owner => self(), client_id => Name}) of
        {ok, Conn} ->
            ["mqtt", M, Node] = string:tokens(Name, "/"),
            Topic  = iolist_to_binary(["/events/1/",Node,"/", M,"/#"]),
            emqtt:connect(Conn),
            emqtt:subscribe(Conn, {Topic, 2}),
            io:format("MQTT worker on ~p initalized. ~n", [Topic]),
            {ok,Async#pi{state=Conn}};
        ignore -> ignore;
        {error, Error} -> {error, Error}
    end;
proc({disconnected, shutdown, tcp_closed}, State) ->
    io:format("MQTT disconnected ~p~n", [State]),
    proc(init,State);

proc({ring, Topic, Request}, State) ->
    io:format("MQTT Ring message ~p~n.", [Request]),
    proc({publish, #{payload => Request, topic => Topic}}, State);

proc({publish, #{payload := Request, topic := <<"/",Address/binary>>}}, State=#pi{state=C}) ->
    [Pre,Vsn,Node,M,_Urs,Cid|_] = string:tokens(binary_to_list(Address),"/"),
    io:format("MQTT Message to {Mod: ~p, Node: ~p,  Cid ~p}.~n", [M, Node, Cid]),
    
    Ctx = #cx{module=list_to_atom(M),node=Node,vsn=Vsn,client_pid=C, params=Cid, from=Address},
    put(context, Ctx),

    case  n2o_proto:try_info(Request,[],Ctx) of
        {reply,{_,      <<>>},_,_}           -> {noreply, State};
        {reply,{bert,   Term},_,#cx{from=X}} -> case send(C,X,n2o_bert:encode(Term)) of 
                                                    ok -> {noreply, State};
                                                    {ok, _Pkid} -> {noreply, State};
                                                    {error, Error} -> {reply, {error, Error}, State} end;
        {reply,{json,   Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,n2o_json:encode(Term))}, State};
        {reply,{text,   Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,Term)}, State};
        {reply,{binary, Term},_,#cx{from=X}} ->

            {reply, {ok,send(C,X,Term)}, State};
        {reply,{default,Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,n2o:encode(Term))}, State};
        {reply,{Encoder,Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,Encoder:encode(Term))}, State};
                                       Reply -> {reply, {error,{"Invalid Return",Reply}}, State}
    end;

proc(Unknown,Async=#pi{name=Name}) ->
    io:format("MQTTv5 [~s]: ~p~n",[Name,Unknown]),
    {reply,{uknown,Unknown,0},Async}.
