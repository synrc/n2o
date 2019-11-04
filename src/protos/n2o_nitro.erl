-module(n2o_nitro).
-description('N2O Nitrogen Web Framework Protocol').
-include_lib("n2o/include/n2o.hrl").
-export([info/3,render_actions/1,io/1,io/2,event/1]).

% Nitrogen pickle handler

info({text,<<"N2O,",Auth/binary>>}, Req, State) ->
    info(#init{token=Auth},Req,State);

info(#init{token=Auth}, Req, State) ->
    {'Token', Token} = n2o_session:authenticate([], Auth),
    Sid = case n2o:depickle(Token) of {{S,_},_} -> S; X -> X end,
    New = State#cx{session = Sid, token = Auth},
    put(context,New),
    {reply,{bert,case io(init, State) of
                      {io,_,{stack,_}} = Io -> Io;
                      {io,Code,_} -> {io,Code,{'Token',Token}} end},
            Req,New};

info(#client{data=Message}, Req, State) ->
    nitro:actions([]),
    {reply,{bert,io(#client{data=Message},State)},Req,State};

info(#pickle{}=Event, Req, State) ->
    nitro:actions([]),
    {reply,{bert,html_events(Event,State)},Req,State};

info(#flush{data=Actions}, Req, State) ->
    nitro:actions(Actions),
    {reply,{bert,io(<<>>)},Req,State};

info(#direct{data=Message}, Req, State) ->
    nitro:actions([]),
    {reply,{bert,case io(Message, State) of
                      {io,_,{stack,_}} = Io -> Io;
                      {io,Code,Res} -> {io,Code,{direct,Res}} end},
            Req,State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

% double render: actions could generate actions

render_actions(Actions) ->
    nitro:actions([]),
    First  = nitro:render(Actions),
    Second = nitro:render(nitro:actions()),
    nitro:actions([]),
    nitro:to_binary([First,Second]).

% n2o events

html_events(#pickle{source=Source,pickled=Pickled,args=Linked}, State=#cx{token = Token}) ->
    Ev  = n2o:depickle(Pickled),
    L   = n2o_session:prolongate(),
    Res = case Ev of
          #ev{} when L =:= false -> render_ev(Ev,Source,Linked,State), <<>>;
          #ev{} -> render_ev(Ev,Source,Linked,State), n2o_session:authenticate([], Token);
          _CustomEnvelop -> %?LOG_ERROR("EV expected: ~p~n",[CustomEnvelop]),
                           {error,"EV expected"} end,
    io(Res).

% calling user code in exception-safe manner

-ifdef(OTP_RELEASE).

render_ev(#ev{module=M,name=F,msg=P,trigger=T},_Source,Linked,State) ->
    try case F of
         api_event -> M:F(P,Linked,State);
             event -> [erlang:put(K,V) || {K,V} <- Linked], M:F(P);
                 _ -> M:F(P,T,State) end
    catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {stack,S} end.

io(Event, #cx{module=Module}) ->
    try X = Module:event(Event), {io,render_actions(nitro:actions()),X}
    catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {io,[],{stack,S}} end.

io(Data) ->
    try {io,render_actions(nitro:actions()),Data}
    catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {io,[],{stack,S}} end.

-else.

render_ev(#ev{module=M,name=F,msg=P,trigger=T},_Source,Linked,State) ->
    try case F of
         api_event -> M:F(P,Linked,State);
             event -> [erlang:put(K,V) || {K,V} <- Linked], M:F(P);
                 _ -> M:F(P,T,State) end
    catch E:R -> S = erlang:get_stacktrace(), ?LOG_EXCEPTION(E,R,S), {stack,S} end.

io(Event, #cx{module=Module}) ->
    try X = Module:event(Event), {io,render_actions(nitro:actions()),X}
    catch E:R -> S = erlang:get_stacktrace(), ?LOG_EXCEPTION(E,R,S), {io,<<>>,{stack,S}} end.

io(Data) ->
    try {io,render_actions(nitro:actions()),Data}
    catch E:R -> S = erlang:get_stacktrace(), ?LOG_EXCEPTION(E,R,S), {io,<<>>,{stack,S}} end.

-endif.

% event Nitrogen Web Framework protocol

event(_) -> [].

