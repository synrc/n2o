-module(n2o_text).
-author('Alexander Salnikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({text,Text}=Message, Req, State) when is_binary(Text) ->
    wf:info(?MODULE,"TEXT Message: ~p",[Message]),
    Module = State#cx.module,
    Resp = try Module:event(Message) catch 
    	E:R -> wf:error(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), <<>> 
    end,
    {reply, Resp, Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
