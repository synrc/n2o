-module(n2o_nitrogen).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

% Nitrogen pickle handler

info({init,Rest},Req,State) ->
    Module = State#cx.module,
    InitActions = case Rest of
         <<>> -> Elements = try Module:main() catch X:Y -> wf:error_page(X,Y) end,
                 wf_render:render(Elements),
                 [];
          Binary -> Pid = wf:depickle(Binary),
                    X = Pid ! {'N2O',self()},
                    receive_actions(Req) end,
    UserCx = try Module:event(init) catch C:E -> wf:error_page(C,E) end,
    Actions = render_actions(wf:actions()),
    self() ! {init_reply,wf:json([{eval,iolist_to_binary([InitActions,Actions])}])},
    {cont,Rest,Req,wf:context(State,?MODULE,UserCx)};


info({text,Message},Req,State) ->   info(Message,Req,State);
info({binary,Message},Req,State) -> info(binary_to_term(Message,[safe]),Req,State);

info({pickle,_,_,_}=Event, Req, State) ->
    wf:actions([]),
    %wf:info(?MODULE,"N2O Message: ~p\n\r",[Event]),
    Result = try html_events(Event,State) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), <<>> end,
    {reply,Result,Req,State};

info({flush,Actions}, Req, State) ->
    wf:actions([]),
    wf:info(?MODULE,"Flush Message: ~p",[Actions]),
    {reply, wf:json([{eval,iolist_to_binary(render_actions(Actions))}]), Req, State};

info({direct,Message}, Req, State) ->
    wf:actions([]),
    Module = State#cx.module,
    Term = try Module:event(Message) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), <<>> end,
    %wf:info(?MODULE,"Direct: ~p Result: ~p",[Message,Term]),
    {reply,wf:json([{eval,iolist_to_binary(render_actions(wf:actions()))}]),Req,State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

% double render: actions could generate actions

render_actions(Actions) ->
    wf:actions([]),
    First  = wf:render(Actions),
    Second = wf:render(wf:actions()),
    wf:actions([]),
    [First,Second].

% N2O events

html_events({pickle,Source,Pickled,Linked}, State) ->
    Ev = wf:depickle(Pickled),
    %wf:info(?MODULE,"Depickled: ~p",[Ev]),
    case Ev of
         #ev{} -> render_ev(Ev,Source,Linked,State);
         CustomEnvelop -> wf:error("Only #ev{} events for now: ~p",[CustomEnvelop]) end,
    wf:json([{eval,iolist_to_binary(render_actions(wf:actions()))}]).

render_ev(#ev{module=M,name=F,msg=P,trigger=T},Source,Linked,State) ->
    case F of
         api_event -> M:F(P,Linked,State);
         event -> lists:map(fun({K,V})-> put(K,V) end,Linked), M:F(P);
         UserCustomEvent -> M:F(P,T,State) end.

receive_actions(Req) ->
    receive 
        {actions,A} -> n2o_nitrogen:render_actions(A);
        _ -> receive_actions(Req)
    after 200 ->
         QS = element(14, Req),
         wf:redirect(case QS of <<>> -> ""; _ -> "?" ++ wf:to_list(QS) end),
         [] end.
