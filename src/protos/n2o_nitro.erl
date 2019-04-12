-module(n2o_nitro).
-description('N2O Nitrogen Web Framework Protocol').
-include("n2o.hrl").
-export([info/3,render_actions/1]).

% Nitrogen pickle handler

info({text,<<"N2O,",Auth/binary>>}, Req, State) ->
    info(#init{token=Auth},Req,State);

info(#init{token=Auth}, Req, State = #cx{module = Module}) ->
     {'Token', Token} = n2o_session:authenticate([], Auth),
     Sid = case n2o:depickle(Token) of {{S,_},_} -> S; X -> X end,
     ?LOG_INFO("N2O SESSION: ~p~n",[Sid]),
     New = State#cx{session = Sid},
     put(context,New),
     {Act,Tok} = try Module:event(init),
                     A = render_actions(nitro:actions()),
                     {A,{'Token',Token}}
               catch E:R:Stack ->
                     ?LOG_ERROR("Catch: ~p:~p~n~p", [E,R,Stack]),
                     {<<>>,{stack,Stack}} end,
     {reply, {bert,{io,Act,Tok}},Req,New};

info(#client{data=Message}, Req, State) ->
    nitro:actions([]),
    ?LOG_INFO("Client Message: ~p",[Message]),
    Module = State#cx.module,
    Reply = try Module:event(#client{data=Message})
          catch E:R:Stack ->
                     ?LOG_ERROR("Catch: ~p:~p~n~p", [E,R,Stack]),
                     {error,Stack} end,
    {reply,{bert,{io,render_actions(nitro:actions()),Reply}},Req,State};

info(#pickle{}=Event, Req, State) ->
    nitro:actions([]),
    Result = try html_events(Event,State)
           catch E:R:Stack ->
                     ?LOG_ERROR("Catch: ~p:~p~n~p", [E,R,Stack]),
                     {io,render_actions(nitro:actions()),Stack} end,
    {reply,{bert,Result}, Req,State};

info(#flush{data=Actions}, Req, State) ->
    Render = nitro:to_binary([render_actions(Actions)]),
    {reply,{bert,{io,Render,<<>>}},Req, State};

info(#direct{data=Message}, Req, State) ->
    nitro:actions([]),
    Module = State#cx.module,
    Result = try Res = Module:event(Message), {direct,Res}
           catch E:R:Stack -> ?LOG_ERROR("Catch: ~p:~p~n~p", [E,R,Stack]),
                              {stack,Stack} end,
    {reply,{bert,{io,render_actions(nitro:actions()),Result}}, Req,State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

% double render: actions could generate actions

render_actions(Actions) ->
    nitro:actions([]),
    First  = nitro:render(Actions),
    Second = nitro:render(nitro:actions()),
    nitro:actions([]),
    nitro:to_binary([First,Second]).

% n2o events

html_events(#pickle{source=Source,pickled=Pickled,args=Linked}, State=#cx{session = Token}) ->
    Ev  = n2o:depickle(Pickled),
    L   = n2o_session:prolongate(),
    Res = case Ev of
          #ev{} when L =:= false -> render_ev(Ev,Source,Linked,State), <<>>;
          #ev{} -> render_ev(Ev,Source,Linked,State), n2o_session:authenticate([], Token);
          CustomEnvelop -> ?LOG_ERROR("EV expected: ~p~n",[CustomEnvelop]),
                           {error,"EV expected"} end,
    {io,render_actions(nitro:actions()),Res}.

render_ev(#ev{name=F,msg=P,trigger=T},_Source,Linked,State=#cx{module=M}) ->
    case F of
         api_event -> M:F(P,Linked,State);
             event -> % io:format("Linked: ~p~n",[Linked]),
                      lists:map(fun ({K,V})-> erlang:put(K,nitro:to_binary([V]))
                                end,Linked),
                      M:F(P);
                 _ -> M:F(P,T,State) end.
