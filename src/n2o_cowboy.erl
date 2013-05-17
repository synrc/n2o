-module(n2o_cowboy).
-author('Roman Shestakov').
-behaviour(cowboy_http_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/3, handle/2, terminate/3]).

-record(state, {headers, body}).

init(_Transport, Req, Opts) -> {ok, Req, #state{}}.
terminate(_Reason, _Req, _State) -> ok.

handle(Req, State) ->
    put(encoder,wf_event:jsonx_encoder()),
    wf_context:init_context(),
    {ok, NewReq} = wf_core:run(Req),
    {ok, NewReq, State}.

