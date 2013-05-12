-module(n2o_SUITE).
-behaviour(cowboy_http_handler).
-include_lib("common_test/include/ct.hrl").
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

suite() -> [{timetrap,{seconds,30}}].
all() -> [{group, elements}].
groups() -> [
	     {elements, [{repeat,10}], [simple_elements]}
	    ].

init_per_suite(Config) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    ok.

init_per_group(elements, Config) ->
    Port = 33084,
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(elements, 100, [{port, Port}], [
								 {env, [{dispatch, init_dispatch()}]},
								 {max_keepalive, 50},
								 {timeout, 500}
								]),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>}, {port, Port}, {opts, []},
     {transport, Transport}, {client, Client} | Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

init_dispatch() ->
    cowboy_router:compile([{"localhost", [{'_', n2o_SUITE, []}]}]).

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.

simple_elements(Config) ->
    Client = ?config(client, Config),
    URL = build_url("/simple_elements", Config),
    ct:log("-> url ~p", [URL]),
    {ok, Client2} = cowboy_client:request(<<"GET">>, URL, Client),
    {ok, 200, Headers, Client3} = cowboy_client:response(Client2),
    {ok, Body, _} = cowboy_client:response_body(Client3),
    ct:log("-> response Body: ~p", [Body]),
    {_, 10} = binary:match(Body, <<"test label">>),
    {_, 12} = binary:match(Body, <<"test textbox">>),
    ok.

%% handle to process http request
-record(state, {headers, body}).
init({_Transport, http}, Req, _Opts) ->
    {ok, Req, #state{}}.
handle(Req, State) ->
    RequestBridge = simple_bridge:make_request(cowboy_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    {ok, NewReq} = nitrogen:run(),
    {ok, NewReq, State}.
terminate(_Reason, _Req, _State) ->
    ok.
