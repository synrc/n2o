-module(web_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib ("n2o/include/wf.hrl").
-include("users.hrl").
-define(APP, web).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, _} = cowboy:start_http(http, 100, [{port, 8000}],
                                           [{env, [{dispatch, dispatch_rules()}]}]),

    users:init(),

    {ok, {{one_for_one, 5, 10}, []}}.

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {["/static/[...]"], cowboy_static, [{directory, {priv_dir, ?APP, [<<"static">>]}},
                                                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
            {["/rest/:bucket"], n2o_rest, []},
            {["/rest/:bucket/:key"], n2o_rest, []},
            {["/rest/:bucket/:key/[...]"], n2o_rest, []},
            {["/websocket/[...]"], n2o_websocket, []},
            {'_', n2o_cowboy, []}
    ]}]).
