-module(n2o_http).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

%% ws.send(enc(tuple(atom('http'), bin(url), bin(method), bin(body), [])));

info(#http{} = Message, Req, State) ->
    wf:info(?MODULE, "Http Message: ~p",[Message]),
    Module = State#cx.module,
    Reply = try Module:event(Message)
          catch E:R -> Error = wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),Reply}),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

