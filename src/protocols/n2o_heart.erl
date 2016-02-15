-module(n2o_heart).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({text,<<"PING">> = _Ping}=Message, Req, State) ->
    wf:info(?MODULE,"PING: ~p",[Message]),
    {reply, <<"PONG">>, Req, State};

info({text,<<"N2O,",Process/binary>> = _InitMarker}=Message, Req, State) ->
    wf:info(?MODULE,"N2O INIT: ~p",[Message]),
    n2o_proto:push({init,Process},Req,State,n2o_proto:protocols(),[]);

% ETS access protocol
info({cache,Operation,Key,Value},Req,State)   -> {reply, case Operation of
                                                              get -> wf:cache(Key);
                                                              set -> wf:cache(Key,Value) end, Req, State};
info({session,Operation,Key,Value},Req,State) -> {reply, case Operation of
                                                              get -> wf:session(Key);
                                                              set -> wf:session(Key,Value) end, Req, State};

% MQ protocol
info({pub,Topic,Message}=Message,Req,State) -> {reply, <<"OK">>, Req, State};
info({sub,Topic,Args}=Message,Req,State)    -> {reply, <<"OK">>, Req, State};
info({unsub,Topic}=Message,Req,State)       -> {reply, <<"OK">>, Req, State};

% WF protocol
info({q,Operation,Key}=Message,Req,State)                -> {reply, <<"OK">>, Req, State};
info({qc,Operation,Key}=Message,Req,State)               -> {reply, <<"OK">>, Req, State};
info({cookie,Operation,Key,Value}=Message,Req,State)     -> {reply, <<"OK">>, Req, State};
info({wire,Parameter}=Message,Req,State)                 -> {reply, <<"OK">>, Req, State};
info({update,Target,Elements}=Message,Req,State)         -> {reply, <<"OK">>, Req, State};
info({insert_top,Target,Elements}=Message,Req,State)     -> {reply, <<"OK">>, Req, State};
info({insert_bottom,Target,Elements}=Message,Req,State)  -> {reply, <<"OK">>, Req, State};

% ASYNC protocol
info({start,Handler}=Message,Req,State)          -> {reply, <<"OK">>, Req, State};
info({stop,Name}=Message,Req,State)              -> {reply, <<"OK">>, Req, State};
info({restart,Name}=Message,Req,State)           -> {reply, <<"OK">>, Req, State};
info({async,Name,Function}=Message,Req,State)    -> {reply, <<"OK">>, Req, State};
info({flush}=Message,Req,State)                  -> {reply, <<"OK">>, Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
