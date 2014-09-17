-module(n2o_client).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({client,Message}, Req, State) ->
    wf:info(?MODULE,"Client Message: ~p",[Message]),
    Module = State#cx.module,
    try Module:event({client,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)) end,
    {reply,wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(wf:actions()))},
                    {data,binary_to_list(term_to_binary(Message))}]),Req,State};

info({server,Message}, Req, State) ->
    wf:info(?MODULE,"Server Message: ~p",[Message]),
    Module = State#cx.module,
    try Module:event({server,Message}),[] catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)) end,
    {reply,wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(wf:actions()))},
                    {data,binary_to_list(term_to_binary(Message))}]),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
