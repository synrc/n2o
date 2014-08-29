-module(n2o_bullet).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

% process spawn

init(_Transport, Req, _Opts, _Active) -> n2o_websocket:init(Req).
info(Data,Req,State) -> n2o_websocket:info(Data, Req, State).
stream(Data, Req, State) -> n2o_websocket:stream(Data, Req, State).
terminate(Req, State) -> n2o_websocket:terminate(Req, State).
