-module(n2o_rails).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({init,Rest},Req,State) ->
    Controller = State#cx.module,
    UserCx = case erlang:function_exported(Controller,init,2) of
         true -> Controller:init(Req,State);
         false -> [] end,
    % you may send here init results back to websocket
    self() ! {init_reply,<<>>},
    wf:info(?MODULE,"n2o_rails:init ~w\r\n",[UserCx]),
    {cont,Rest,Req,wf:context(State,?MODULE,UserCx)};

info({text,Message},Req,State) ->    info(Message,Req,State);
info({binary,Message},Req,State) ->  info(binary_to_term(Message,[safe]),Req,State);

info({rails,_,_,_}=Event, Req, State) ->
    wf:info(?MODULE,"n2o_rails:pickle: ~p",[Event]),
    wf:actions([]),
    {Result,NewState} = 
         try events(Event,State) 
         catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), {<<>>,State} end,
    {reply,Result,Req,NewState};

info({flush,Actions}, Req, State) ->
    wf:actions([]),
    wf:info(?MODULE,"Flush Message: ~p",[Actions]),
    {reply, wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(Actions))}]), Req, State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

events({rails,Source,Pickled,Linked}, State) -> handle_ev(Source,wf:depickle(Pickled),Linked,State);
events({direct,Source,Ev,Linked}, State) -> handle_ev(Source,Ev,Linked,State).

handle_ev(Source,Ev,Linked,State) ->
    wf:info(?MODULE,"n2o_rails:rails_events: ~p",[Ev]),
    case Ev of
         #ev{}         -> render_ev(Ev,Source,Linked,State);
         CustomEnvelop -> wf:error("Only #ev{} events for now: ~p",[CustomEnvelop]),
                          {<<>>,State} end.

render_ev(#ev{module=Controller,name=Action,msg=P,trigger=T}=Ev,Source,Linked,State) ->
    case Controller:Action(Ev,State#cx{params=Linked}) of
         {json,Dictionary,NewState} -> {wf:json(Dictionary),NewState};
         {binary,Raw,NewState} -> {{binary,Raw},NewState};
         {actions,Elements,NewState} -> {wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(wf:actions()))}]), NewState};
         {file,FileName,NewState} -> {<<>>,NewState};
         {redirect,Address,NewState} -> {<<>>,NewState};
         _ -> {<<>>,State}
    end.
