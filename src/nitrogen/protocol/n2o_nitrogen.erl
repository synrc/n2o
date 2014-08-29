-module(n2o_nitrogen).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

% Nitrogen pickle handler

info({text,Message},Req,State) -> 
%    wf:info(?MODULE,"n2o_nitrogen:text ~w",[Message]),
    info(binary_to_term(Message,[safe]),Req,State);
info({binary,Message},Req,State) -> 
%    wf:info(?MODULE,"n2o_nitrogen:binary ~w",[Message]),
    info(binary_to_term(Message,[safe]),Req,State);

info({pickle,_,_,_}=Event, Req, State) ->
    wf_context:clear_actions(),
%    wf:info(?MODULE,"N2O Message: ~p",[Event]),
    Result = try html_events(Event,State) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), wf:json([]) end,
%    wf:info(?MODULE,"Pickle Cookies: ~p",[wf_core:set_cookies(wf:cookies(),Req)]),
    {reply,Result,wf_core:set_cookies(wf:cookies(),Req),State};

info({flush,Actions}, Req, State) ->
    wf_context:clear_actions(),
    wf:info(?MODULE,"Flush Message: ~p",[Actions]),
    {reply, wf:json([{eval,iolist_to_binary(render_actions(Actions))}]), Req, State};

info({delivery,Route,Message}, Req, State) ->
    wf_context:clear_actions(),
    Module = State#context.module,
    Term = try Module:event({delivery,Route,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), <<>> end,
    wf:info(?MODULE,"Delivery: ~p Result: ~p",[Message,Term]),
    {reply,wf:json([{eval,iolist_to_binary(render_actions(get(actions)))}]),Req,State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

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

