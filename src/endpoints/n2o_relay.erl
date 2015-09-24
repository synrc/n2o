-module(n2o_relay).
-description('N2O TCP relay endpoint handler').
-compile(export_all).

connect(IP, PortNo) ->
    {ok, Socket} = gen_tcp:connect(IP, PortNo, [{active, false}, {packet, 2}]),
    spawn(fun() -> recv(Socket) end),
    Socket.

send(Socket, Message) ->
    BinMsg = term_to_binary(Message),
    gen_tcp:send(Socket, BinMsg).

recv(Socket) ->
    {ok, A} = gen_tcp:recv(Socket, 0),
    io:format("Received: ~p~n", [A]),
    recv(Socket).

disconnect(Socket) ->
    gen_tcp:close(Socket).
