-module(n2o_bullet).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").

-compile(export_all).
-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

% process spawn

init(_Transport, Req, _Opts, _Active) ->
    put(actions,[]),
    Ctx = wf_context:init_context(Req),
    NewCtx = wf_core:fold(init,Ctx#context.handlers,Ctx),
    wf_context:context(NewCtx),
    Res = ets:update_counter(globals,onlineusers,{2,1}),
    wf:reg(broadcast,wf:peer(Req)),
    wf:send(broadcast,{counter,Res}),
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

info({client,Message}, Req, State) ->
    wf_context:clear_actions(),
%    wf:info(?MODULE,"Client Message: ~p",[Message]),
    Module = State#context.module,
    try Module:event({client,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", [E,R,n2o_error:stack()]) end,
    {reply,wf:json([{eval,iolist_to_binary(render_actions(get(actions)))},
                    {data,binary_to_list(term_to_binary(Message))}]),Req,State};

info({bert,Message}, Req, State) ->
    wf_context:clear_actions(),
    Module = State#context.module,
    Term = try Module:event({bert,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", [E,R,n2o_error:stack()]), <<>> end,
    wf:info(?MODULE,"Client BERT Binary Message: ~p Result: ~p",[Message,Term]),
    {reply,{binary,term_to_binary(Term)},Req,State};

info({binary,Message}, Req, State) ->
    wf_context:clear_actions(),
    Module = State#context.module,
    Term = try Module:event({binary,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", [E,R,n2o_error:stack()]), <<>> end,
    wf:info(?MODULE,"Client Raw Binary Message: ~p Result: ~p",[Message,Term]),
    Res = case Term of _ when is_binary(Term) -> Term; _ -> term_to_binary(Term) end,
    {reply,{binary,Res},Req,State};

info({server,Message}, Req, State) ->
    wf_context:clear_actions(),
%    wf:info(?MODULE,"Server Message: ~p",[Message]),
    Module = State#context.module,
    try Module:event({server,Message}),[] catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", [E,R,n2o_error:stack()]) end,
    {reply,wf:json([{eval,iolist_to_binary(render_actions(get(actions)))},
                    {data,binary_to_list(term_to_binary(Message))}]),Req,State};

info({pickle,_,_,_}=Event, Req, State) ->
    wf_context:clear_actions(),
%    wf:info(?MODULE,"N2O Message: ~p",[Event]),
    Result = try html_events(Event,State) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", [E,R,n2o_error:stack()]), wf:json([]) end,
    wf:info(?MODULE,"Pickle Cookies: ~p",[wf_core:set_cookies(wf:cookies(),Req)]),
    {reply,Result,wf_core:set_cookies(wf:cookies(),Req),State};

info({flush,Actions}, Req, State) ->
    wf_context:clear_actions(),
    wf:info(?MODULE,"Flush Message: ~p",[Actions]),
    {reply, wf:json([{eval,iolist_to_binary(render_actions(Actions))}]), Req, State};

info({delivery,Route,Message}, Req, State) ->
    wf_context:clear_actions(),
    Module = State#context.module,
    Term = try Module:event({delivery,Route,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", [E,R,n2o_error:stack()]), <<>> end,
    wf:info(?MODULE,"Delivery: ~p Result: ~p",[Message,Term]),
    {reply,wf:json([{eval,iolist_to_binary(render_actions(get(actions)))}]),Req,State};

info(<<"PING">> = Ping, Req, State) ->
%    wf:info(?MODULE,"Ping Message: ~p",[Ping]),
    {reply, wf:json([]), Req, State};

info(<<"N2O,",Rest/binary>> = InitMarker, Req, State) ->
    wf:info(?MODULE,"N2O INIT: ~p",[Rest]),
    Module = State#context.module,
    InitActions = case Rest of
         <<>> -> Elements = try Module:main() catch X:Y -> wf:error_page(X,Y) end,
                 wf_core:render(Elements),
                 [];
          Binary -> Pid = wf:depickle(Binary), % FIXME TODO Legacy Nitrogen Compatible Code should be more pretty
                    X = Pid ! {'N2O',self()},
                    R = receive A -> render_actions(A) after 100 ->
                        QS = element(14, Req),
                        wf:redirect(case QS of <<>> -> ""; _ -> "?" ++ wf:to_list(QS) end),
                        []
                    end,
                    R end,
    try Module:event(init) catch C:E -> wf:error_page(C,E) end,
    Actions = wf:render(get(actions)),
    {reply, wf:json([{eval,iolist_to_binary([InitActions,Actions])}]), Req, State};

info(Unknown, Req, State) ->
    wf:info(?MODULE,"Unknown Message: ~p",[Unknown]),
    Module = State#context.module,
    try Module:event(Unknown) catch C:E -> wf:error_page(C,E) end,
    {reply, wf:json([{eval,iolist_to_binary(render_actions(get(actions)))}]), Req, State}.

% double render: actions could generate actions

render_actions(Actions) ->
    wf_context:clear_actions(),
    First  = wf:render(Actions),
    Second = wf:render(get(actions)),
    wf_context:clear_actions(),
    [First,Second].

% N2O events

html_events({pickle,Source,Pickled,Linked}, State) ->
    Ev = wf:depickle(Pickled),
%    wf:info(?MODULE,"Depickled: ~p",[Ev]),
    case Ev of
         #ev{} -> render_ev(Ev,Source,Linked,State);
         CustomEnvelop -> wf:error("Only #ev{} events for now: ~p",[CustomEnvelop]) end,
    wf:json([{eval,iolist_to_binary(render_actions(get(actions)))}]).

render_ev(#ev{module=M,name=F,payload=P,trigger=T},Source,Linked,State) ->
    case F of 
         api_event -> M:F(P,Linked,State);
         event -> lists:map(fun({K,V})-> put(K,V) end,Linked), M:F(P);
         UserCustomEvent -> M:F(P,T,State) end.

% process down

terminate(_Req, _State=#context{module=Module}) ->
    Res = ets:update_counter(globals,onlineusers,{2,-1}),
    wf:send(broadcast,{counter,Res}),
    catch Module:event(terminate),
    ok.
