-module(n2o_query_handler).
-author('Maxim Sokhatsky').
-behaviour(query_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

init(State, Ctx) -> 
    Params = wf:params(Ctx#context.req),
%    error_logger:info_msg("Params: ~p",[Params]),
    wf_context:params(Params),
    {ok, [], Ctx#context{params=Params}}.

finish(State, Ctx) ->  {ok, [], Ctx}.
