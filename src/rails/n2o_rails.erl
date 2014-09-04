-module(n2o_rails).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({text,Message},Req,State) ->    info(Message,Req,State);
info({binary,Message},Req,State) ->  info(binary_to_term(Message,[safe]),Req,State);

info({pickle,_,_,_}=Event, Req, State) ->
    wf_context:clear_actions(),
    {Result,NewState} = 
         try rails_events(Event,State) 
         catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), {<<>>,State} end,
    {reply,Result,wf_core:set_cookies(wf:cookies(),Req),State};

info({flush,Actions}, Req, State) ->
    wf_context:clear_actions(),
    wf:info(?MODULE,"Flush Message: ~p",[Actions]),
    {reply, wf:json([{eval,iolist_to_binary(render_actions(Actions))}]), Req, State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

render_actions(Actions) ->
    wf_context:clear_actions(),
    First  = wf:render(Actions),
    Second = wf:render(get(actions)),
    wf_context:clear_actions(),
    [First,Second].

rails_events({pickle,Source,Pickled,Linked}, State) ->
    Ev = wf:depickle(Pickled),
    case Ev of
         #ev{}         -> render_ev(Ev,Source,Linked,State);
         CustomEnvelop -> wf:error("Only #ev{} events for now: ~p",[CustomEnvelop]),
                          {<<>>,State} end.

render_ev(#ev{module=Controller,name=Action,payload=P,trigger=T}=Ev,Source,Linked,State) ->
    case Controller:Action(Ev,State#context{params=Linked}) of
         {json,Dictionary,NewState} -> {wf:json(Dictionary),NewState};
         {binary,Raw,NewState} -> {{binary,Raw},NewState};
         {render,Elements,NewState} -> {[],NewState};
         {file,FileName,NewState} -> {<<>>,NewState};
         {redirect,Address,NewState} -> {<<>>,NewState}
    end.
