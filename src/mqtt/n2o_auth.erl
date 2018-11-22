-module(n2o_auth).
-description('N2O MQTT Auth').
-include("emqttd.hrl").
-export([init/1, check/3, description/0]).

init([Listeners]) -> {ok, Listeners}.
get_client_id() -> {_, NPid, _} = emqttd_guid:new(), n2o:to_binary(["emqttd_", integer_to_list(NPid)]).
description() -> "N2O Authentication Module".

check(#mqtt_client{client_id = ClientId,
                    username  = PageModule,
                    client_pid = ClientPid,
                    ws_initial_headers = _Headers},
            _Password, _Listeners) ->
    ClientId2 = case ClientId of <<>> -> get_client_id(); _ ->  ClientId end,
    case ClientId2 of
        <<"emqttd_", _/binary>> ->
            emqttd_client:subscribe(ClientPid,
                [{n2o:to_binary(["actions/1/",PageModule,"/",ClientId2]), 2}]),
            ignore;
        _ -> ignore
    end;
check(_Client, _Password, _Opts) -> ignore.

