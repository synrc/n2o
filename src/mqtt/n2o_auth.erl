-module(n2o_auth).
-description('N2O MQTT Auth').
-include("emqttd.hrl").
%-behaviour(emqttd_auth_mod).
-compile(export_all).
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

ttl() -> (application:get_env(n2o, auth_ttl, 60*15)).
gen_token([], _Data) ->
    Now = now_msec(),
    Expiration = Now+ttl()*1000,
    {'Token', n2o_secret:pickle(term_to_binary(Expiration))};
gen_token(ClientSessionToken, Data) ->
    Now = now_msec(),
    case bin_to_term(n2o_secret:depickle(ClientSessionToken)) of
        <<>> -> {error, invalid_token};
        Expiration when Expiration > Now -> {'Token', ClientSessionToken};
        _Expiration -> gen_token([], Data)
    end.

bin_to_term(<<>>) -> <<>>;
bin_to_term(Bin) -> binary_to_term(Bin).

gen_sid(Time) ->
    nitro_conv:hex(binary:part(crypto:hmac(application:get_env(n2o,hmac,sha256),
        n2o_secret:secret(),term_to_binary(Time)),0,16)).

now_msec() -> now_msec(os:timestamp()).
now_msec({Mega,Sec,Micro}) -> (Mega*1000000 + Sec)*1000 + round(Micro/1000).
msec_now(A) -> A0 = A/1000, S = trunc(A0), Mega = S div 1000000,
                Sec = S - Mega*1000000, Micro = round((A0 - S)*1000000), {Mega,Sec,Micro}.
