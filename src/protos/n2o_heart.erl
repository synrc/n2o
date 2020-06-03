-module(n2o_heart).
-description('N2O Heartbeat Protocol').
-include_lib("n2o/include/n2o.hrl").
-export([info/3]).

info({text,<<"PING">> = _Ping}, Req, State) ->
    {reply, {text, <<"PONG">>}, Req, State};

info({text,<<>>}, Req, State) ->
    {reply, {text, <<>>}, Req, State};

info(Message, Req, State) ->
    {unknown,Message, Req, State}.
