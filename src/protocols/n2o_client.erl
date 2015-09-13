-module(n2o_client).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({client,Message}, Req, State) ->
    wf:info(?MODULE,"Client Message: ~p",[Message]),
    Module = State#cx.module,
    Reply = try Module:event({client,Message})
          catch E:R -> Error = wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),Reply}),Req,State};

info({server,Message}, Req, State) ->
    wf:info(?MODULE,"Server Message: ~p",[Message]),
    Module = State#cx.module,
    Reply  = try Module:event({server,Message})
           catch E:R -> Error = wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),Reply}),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
