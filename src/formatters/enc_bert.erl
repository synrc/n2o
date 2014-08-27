-module(enc_bert).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").

info({bert,Message}, Req, State) ->
    wf_context:clear_actions(),
    Module = State#context.module,
    Term = try Module:event({bert,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), <<>> end,
    wf:info(?MODULE,"Client BERT Binary Message: ~p Result: ~p",[Message,Term]),
    {reply,{binary,term_to_binary(Term)},Req,State}.
