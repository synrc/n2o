-module(n2o_heart).
-description('N2O Heartbeat Protocol').
-export([info/3]).

info({text,<<"PING">> = _Ping}=Message, Req, State) ->
    n2o:info(?MODULE,"PING: ~p",[Message]),
    {reply, {text, <<"PONG">>}, Req, State};

info({text,<<>>}=Message, Req, State) ->
    n2o:info(?MODULE,"NOP: ~p",[Message]),
    {reply, {text, <<>>}, Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
