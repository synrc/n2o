-module(n2o_vnode).
-description('N2O MQTT Backend').
-include("n2o.hrl").
-compile(export_all).

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

send(C,T,M)      -> send(C, T, M, [{qos,2}]).
send(C,T,M,Opts) -> emqttc:publish(C, T, M, Opts).

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
        Sid  = n2o:to_binary(Token),
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
