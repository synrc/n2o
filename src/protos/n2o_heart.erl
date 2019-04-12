-module(n2o_heart).
-description('N2O Heartbeat Protocol').
-include("n2o.hrl").
-export([info/3]).

info({text,<<"PING">> = _Ping}=Message, Req, State) ->
    ?LOG_INFO("PING: ~p",[Message]),
    {reply, {text, <<"PONG">>}, Req, State};

info({text,<<>>}=Message, Req, State) ->
    ?LOG_INFO("NOP: ~p",[Message]),
    {reply, {text, <<>>}, Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
