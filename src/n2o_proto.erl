-module(n2o_proto).

-description('N2O WebSocket Protocol').

-include_lib("n2o/include/n2o.hrl").

-export([init/2,
         finish/2,
         info/3,
         info/4,
         stream/3,
         push/5,
         init/4,
         terminate/2,
         cx/2]).

-export([try_info/4, try_info/5]).

-export([protocols/0]).

% Common protocol for MQTT, TCP and WebSocket

protocols() ->
    application:get_env(n2o, protocols, [n2o_heart]).

info(M, R, S) -> push(M, R, S, protocols(), []).

info(M, R, S, Ps) -> push(M, R, S, Ps, []).

nop(R, S) -> {reply, {binary, <<>>}, R, S}.

reply(M, R, S) -> {reply, M, R, S}.

push(_, R, S, [], _) -> nop(R, S);
push(M, R, S, [H], _) ->
    case H:info(M, R, S) of
        {reply, M1, R1, S1} -> reply(M1, R1, S1);
        {unknown, _, _, _} -> nop(R, S)
    end;
push(M, R, S, [H | T], Acc) ->
    case H:info(M, R, S) of
        {unknown, _, _, _} -> push(M, R, S, T, Acc);
        {reply, M1, R1, S1} -> reply(M1, R1, S1);
        A -> push(M, R, S, T, [A | Acc])
    end.

cx(Cookies, Req) ->
    Token = case lists:keyfind(<<"X-Auth-Token">>,
                               1,
                               Cookies)
                of
                {_, V} -> V;
                false -> <<>>
            end,
    Sid = case n2o:depickle(Token) of
              {{S, _}, _} -> S;
              _ -> <<>>
          end,
    #cx{actions = [], path = [], req = Req, params = [],
        session = Sid, token = Token,
        handlers =
            [{routes, application:get_env(n2o, routes, ?MODULE)}]}.

% Part of WebSocket Adapter

finish(State, Cx) -> {ok, State, Cx}.

init(State, Cx) -> {ok, State, Cx}.

fold(Fun, Handlers, Ctx) ->
    lists:foldl(fun ({_, []}, C) -> C;
                    ({_, Module}, Ctx1) ->
                        {ok, _, NewCtx} = Module:Fun([], Ctx1),
                        NewCtx
                end,
                Ctx,
                Handlers).

terminate(_, #cx{module = Module}) ->
    catch Module:event(terminate).

init(_Transport, Req, _Opts, _) ->
    Cookies = cowboy_req:parse_cookies(Req),
    {Module, CxInit} = application:get_env(n2o,
                                           cx,
                                           {n2o_proto, cx}),
    Zero = Module:CxInit(Cookies, Req),
    put(context, Zero),
    Ctx = fold(init, Zero#cx.handlers, Zero),
    put(context, Ctx),
    Origin = case cowboy_req:header(<<"origin">>,
                                    Req,
                                    <<"*">>)
                 of
                 {O, _} -> O;
                 X -> X
             end,
    ConfigOrigin = iolist_to_binary(application:get_env(n2o,
                                                        origin,
                                                        Origin)),
    Req1 =
        cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>,
                                   ConfigOrigin,
                                   Ctx#cx.req),
    {ok, Req1, Ctx}.

stream({text, _} = M, R, S) ->
    push(M, R, S, protocols(), []);
stream({binary, <<>>}, R, S) -> nop(R, S);
stream({binary, D}, R, S) ->
    push(n2o:decode(D), R, S, protocols(), []);
stream(_, R, S) -> nop(R, S).

try_info(M, R, S, Ps) -> try_info(?MODULE, M, R, S, Ps).

-ifdef(OTP_RELEASE).

try_info(Module, M, R, S, Ps) ->
    try Module:info(M, R, S, Ps) catch
        Err:Rea:Stack ->
            ?LOG_EXCEPTION(Err, Rea, Stack),
            {error, {stack, Stack}}
    end.

-else.

try_info(Module, M, R, S, Ps) ->
    try Module:info(M, R, S, Ps) catch
        Err:Rea ->
            Stack = erlang:get_stacktrace(),
            ?LOG_EXCEPTION(Err, Rea, Stack),
            {error, {stack, Stack}}
    end.

-endif.
