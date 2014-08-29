-module(n2o_bert).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").

info({binary,{bert,Message}}, Req, State) -> info({bert,Message}, Req, State);

info({bert,Message}, Req, State) ->
    wf_context:clear_actions(),
    Module = State#context.module,
    Term = try Module:event({bert,Message}) catch E:R -> wf:info(?MODULE,"Catch: ~p:~p~n~p", wf:stack(E, R)), <<>> end,
    wf:info(?MODULE,"Client BERT Binary Message: ~p Result: ~p",[Message,Term]),
    {reply,{binary,term_to_binary(Term)},Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

encode(Message) -> term_to_binary(Message).
