-module(nitrogen_cowboy).
-behaviour(cowboy_http_handler).
-include_lib("nitrogen_core/include/wf.hrl").
-export([init/3, handle/2, terminate/3]).

-record(state, {headers, body}).

init(_Transport, Req, Opts) -> {ok, Req, #state{}}.
terminate(_Reason, _Req, _State) -> ok.

handle(Req, State) ->
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    {ok, NewReq} = nitrogen:run(),
    {ok, NewReq, State}.
