-module(n2o_websocket).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

%    I made this for you to be happy.

%    You can plug any binary formatting protocol like BERT, BED, MessagePack
%    or even textual ones like TXT JSON or Custom Binary Protocols

%    Nitrogen-support here is just thre protocol messages: pickle, flush and delivery.
%    N2O provides also basic Heartbeat protocol that can be formatted at your whim.

init(_Transport, Req, _Opts, _Active) ->
    put(actions,[]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_core:fold(init,Ctx#context.handlers,Ctx),
    wf_context:context(NewCtx),
    wf:reg(broadcast,{wf:peer(Req)}),
    Req1 = wf:header(<<"Access-Control-Allow-Origin">>, <<"*">>, NewCtx#context.req),
    {ok, Req1, NewCtx}.

% websocket handler

stream(<<"PING">> =Ping, Req, State) -> info(Ping,Req,State);
stream(<<"N2O",Rest/binary>> = Data, Req, State) -> info(Data,Req,State);
stream({text,Data}, Req, State) -> info(Data,Req,State);
stream({binary,Info}, Req, State) -> info(binary_to_term(Info,[safe]),Req,State);
stream(Data, Req, State) when is_binary(Data) -> info(binary_to_term(Data,[safe]),Req,State);
stream(Data, Req, State) -> info(Data,Req,State).

% WebSocketPid ! Message

protocols() -> wf:config(n2o,protocols,[ n2o_heart,n2o_nitrogen,enc_bert,enc_binary,enc_json ]).

info(Message, Req, State) ->
    Answers = [ { P,P:handle(#ev{payload=Message},State) } || P <- protocols() ],
    case Result of
        { reply, PropList } -> 
        { reply,
            xx:protocol_reply(json,
            [{eval,iolist_to_binary(Actions)},
             {data,binary_to_list(term_to_binary(Message))}],
            Req,
            State } end.

% N2O nitrogen pickle protocol

info({pickle,_,_,_}=Event, Req, State) -> n2o_nitrogen:info(Event,Req,State);
info({flush,_}=Event, Req, State) -> n2o_nitrogen:info(Event,Req,State);
info({delivery,Route,Message}, Req, State) -> n2o_nitrogen:info(Event,Req,State);

% N2O heart protocol

info(<<"PING">> = Ping, Req, State) -> n2o_heart:info(Ping,Req,State);
info(<<"N2O,",Rest/binary>>=InitMarker,Req,State) -> n2o_heart:info(InitMarker,Req,State);

info(Unknown, Req, State) ->
    wf:info(?MODULE,"Unknown Message: ~p",[Unknown]),
    Module = State#context.module,
    try Module:event(Unknown) catch C:E -> wf:error_page(C,E) end,
    {reply, wf:json([]), Req, State}.

% process down

terminate(_Req, _State=#context{module=Module}) ->
    catch Module:event(terminate).
