-module(n2o_mqtt).
-export([proc/2,send/3]).
-include_lib("n2o/include/n2o.hrl").

send(C,T,M) ->
    emqtt:publish(C,T,M).

proc(init,#pi{name=Name}=Async) ->
    case emqtt:start_link(#{owner => self(), client_id => Name}) of
        {ok, Conn} ->
            emqtt:connect(Conn),
            emqtt:subscribe(Conn, {list_to_binary(Name),2}),
            io:format("MQTT worker on ~p initalized. ~n", [Name]),
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
    [Proto,App,Node|_] = string:tokens(binary_to_list(Address),"/"),
    put(context,Cx=#cx{module=App,node=Node,params=Proto,client_pid=C,from= <<"/response/",Address/binary>>}),
    
    io:format("MQTTv5 [~p]: Req: ~p~n",[App,Request]),
    case  n2o_proto:try_info(Request,[],Cx) of
        {reply,{_,      <<>>},_,_}           -> {noreply, State};
        {reply,{bert,   Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,n2o_bert:encode(Term))}, State};
        {reply,{json,   Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,n2o_json:encode(Term))}, State};
        {reply,{text,   Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,Term)}, State};
        {reply,{binary, Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,Term)}, State};
        {reply,{default,Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,n2o:encode(Term))}, State};
        {reply,{Encoder,Term},_,#cx{from=X}} -> {reply, {ok,send(C,X,Encoder:encode(Term))}, State};
                                       Reply -> {reply, {error,{"Invalid Return",Reply}}, State}
    end;

proc(Unknown,Async=#pi{name=Name}) ->
    io:format("MQTTv5 [~s]: ~p~n",[Name,Unknown]),
    {reply,{uknown,Unknown,0},Async}.
