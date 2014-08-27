-module(enc_json).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").

info({client,Message}, Req, State) ->
    wf_context:clear_actions(),
%    wf:info(?MODULE,"Client Message: ~p",[Message]),
    Module = State#context.module,
    try Module:event({client,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)) end,
    {reply,wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(get(actions)))},
                    {data,binary_to_list(term_to_binary(Message))}]),Req,State};

info({server,Message}, Req, State) ->
    wf_context:clear_actions(),
%    wf:info(?MODULE,"Server Message: ~p",[Message]),
    Module = State#context.module,
    try Module:event({server,Message}),[] catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)) end,
    {reply,wf:json([{eval,iolist_to_binary(n2o_nitrogen:render_actions(get(actions)))},
                    {data,binary_to_list(term_to_binary(Message))}]),Req,State}.

