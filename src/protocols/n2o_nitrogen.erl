-module(n2o_nitrogen).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

% Nitrogen pickle handler

info({init,Rest},Req,State) ->
    Module = State#cx.module,
    InitActionsReply = case Rest of
         <<>> -> try Elements = Module:main(),
                     wf_render:render(Elements),
                     {ok,[]}
               catch X:Y -> Stack = wf:stack(X,Y),
                            wf:error(?MODULE,"Event Main: ~p:~p~n~p", Stack),
                            {error,Stack} end;
          Binary -> Pid = wf:depickle(Binary),
                    Pid ! {'N2O',self()},
                    {ok,receive_actions(Req)} end,
    case InitActionsReply of
         {ok,InitActions} -> UserCx = try Module:event(init),
                                     case wf:config(n2o,auto_session) of
                              disabled -> skip;
                                      _ -> n2o_session:ensure_sid([],?CTX,[]) end
                                    catch C:E -> Error = wf:stack(C,E),
                                                 wf:error(?MODULE,"Event Init: ~p:~p~n~p",Error),
                                                 {stack,Error} end,
                             Actions = render_actions(wf:actions()),
                             {reply,wf:format({io,iolist_to_binary([InitActions,Actions]),<<>>}),
                                    Req,wf:context(State,?MODULE,{init,UserCx})};
           {error,E} ->  {reply,wf:format({io,<<>>,E}), Req, wf:context(State,?MODULE,{error,E})} end;

info({pickle,_,_,_}=Event, Req, State) ->
    wf:actions([]),
    Result = try html_events(Event,State)
           catch E:R -> Stack = wf:stack(E,R),
                        wf:error(?MODULE,"Catch: ~p:~p~n~p", Stack),
                        {io,render_actions(wf:actions()),Stack} end,
    {reply,wf:format(Result),Req,wf:context(State,?MODULE,{pickle,Result})};

info({flush,Actions}, Req, State) ->
    wf:actions([]),
    Render = iolist_to_binary(render_actions(Actions)),
    wf:info(?MODULE,"Flush Message: ~tp",[Render]),
    {reply,wf:format({io,Render,<<>>}),Req, State};

info({direct,Message}, Req, State) ->
    wf:actions([]),
    Module = State#cx.module,
    Result = try Res = Module:event(Message), {direct,Res}
           catch E:R -> Stack = wf:stack(E, R),
                        wf:error(?MODULE,"Catch: ~p:~p~n~p", Stack),
                        {stack,Stack} end,
    {reply,wf:format({io,render_actions(wf:actions()),<<>>}),Req, wf:context(State,?MODULE,Result)};

info(Message,Req,State) -> {unknown,Message,Req,State}.

% double render: actions could generate actions

render_actions(Actions) ->
    wf:actions([]),
    First  = wf:render(Actions),
    Second = wf:render(wf:actions()),
    wf:actions([]),
    [First,Second].

% N2O events

html_events({pickle,Source,Pickled,Linked}=Pickle, State) ->
    wf:info(?MODULE,"Pickle: ~tp",[Pickle]),
    Ev = wf:depickle(Pickled),
    Result = case Ev of
         #ev{} -> render_ev(Ev,Source,Linked,State);
         CustomEnvelop -> wf:error("Only #ev{} events for now: ~p",[CustomEnvelop]) end,
    {io,render_actions(wf:actions()),<<>>}.

render_ev(#ev{module=M,name=F,msg=P,trigger=T},_Source,Linked,State) ->
    case F of
         api_event -> M:F(P,Linked,State);
         event -> lists:map(fun({K,V})-> put(K,wf:to_binary(V)) end,Linked), M:F(P);
         _UserCustomEvent -> M:F(P,T,State) end.

receive_actions(Req) ->
    receive
        {actions,A} -> n2o_nitrogen:render_actions(A);
        _ -> receive_actions(Req)
    after 200 ->
         QS = element(14, Req),
         wf:redirect(case QS of <<>> -> ""; _ -> "?" ++ wf:to_list(QS) end),
         [] end.
