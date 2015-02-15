-module(n2o_jsonp).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({json,Message}, Req, State) when is_binary(Message) ->
    wf:info(?MODULE,"JSON Message: ~p",[Message]),
    Module = State#cx.module,
    try Module:event({json, n2o_json:decode(Message)}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)) 
    end,
    {reply,wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(wf:actions()))}]),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
