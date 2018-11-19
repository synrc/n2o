-module(n2o_nitro).
-description('N2O Nitro Protocol').
-include("n2o.hrl").
-compile(export_all).

% Nitrogen pickle handler

info({text,<<"N2O,",Auth/binary>>}, Req, State) ->
    {'Token', Token} = n2o_session:authenticate([], Auth), % WS
    info(#init{token=Token},Req,State#cx{session = Token});

info(#init{token=Token}, Req, State = #cx{module = Module, session = _Session}) ->
    Bin = binary:part(Token,0,20),
    n2o:info(?MODULE,"N2O TOKEN: ~p~n",[<<"N2O,",Bin/binary>>]),
     case try Elements = Module:main(),
              nitro:render(Elements),
              {ok,[]}
        catch Err:Rea ->
              StackMain = n2o:stack_trace(Err,Rea),
              n2o:error(?MODULE,"Catch:~p~n",[StackMain]),
              {error,StackMain} end of
   {ok, _} -> Actions = try Module:event(init),
                            render_actions(nitro:actions())
         catch Err1:Rea1 -> StackInit = n2o:stack_trace(Err1,Rea1),
                            n2o:error(?MODULE,"Catch:~p~n",[StackInit]),
                            {stack,StackInit} end,
              {reply, {bert,{io,Actions,{'Token',Token}}},Req,State};
 {error,E} -> {reply, {bert,{io,<<>>,E}},Req,State} end;

info(#client{data=Message}, Req, State) ->
    nitro:actions([]),
    n2o:info(?MODULE,"Client Message: ~p",[Message]),
    Module = State#cx.module,
    Reply = try Module:event(#client{data=Message})
          catch Err:Rea -> Stack = n2o:stack_trace(Err,Rea),
                           n2o:error(?MODULE,"Catch:~p~n",[Stack]),
                           {error,Stack} end,
    {reply,{bert,{io,render_actions(nitro:actions()),Reply}},Req,State};

info(#pickle{}=Event, Req, State) ->
    nitro:actions([]),
    Result = try html_events(Event,State)
           catch E:R -> Stack = n2o:stack_trace(E,R),
                        n2o:error(?MODULE,"Catch: ~p:~p~n~p", Stack),
                        {io,render_actions(nitro:actions()),Stack} end,
    {reply,{bert,Result}, Req,State};

info(#flush{data=Actions}, Req, State) ->
    Render = nitro:to_binary([render_actions(Actions)]),
    {reply,{bert,{io,Render,<<>>}},Req, State};

info(#direct{data=Message}, Req, State) ->
    nitro:actions([]),
    Module = State#cx.module,
    Result = try Res = Module:event(Message), {direct,Res}
           catch E:R -> Stack = n2o:stack_trace(E, R),
                        n2o:error(?MODULE,"Catch: ~p:~p~n~p", Stack),
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
    Res = case Ev of
          #ev{} -> render_ev(Ev,Source,Linked,State),
                   case application:get_env(n2o,nitro_prolongate,no) of no -> <<>>;
                               _ -> n2o_session:authenticate([], Token) end;
          CustomEnvelop -> n2o:error(?MODULE,"EV expected: ~p~n",[CustomEnvelop]),
                           {error,"EV expected"} end,
    {io,render_actions(nitro:actions()),Res}.

render_ev(#ev{name=F,msg=P,trigger=T},_Source,Linked,State=#cx{module=M}) ->
    case F of
         api_event -> M:F(P,Linked,State);
             event -> lists:map(fun ({K,V})-> erlang:put(K,nitro:to_binary([V]))
                                end,Linked),
                      M:F(P);
                 _ -> M:F(P,T,State) end.
