-module(web_sup).
-behaviour(supervisor).
-export([
	 start_link/0,
	 init/1
	]).

-compile(export_all).
-include_lib ("n2o/include/wf.hrl").
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
    {ok, _} = cowboy:start_http(http, 10000,
				[{port, 8000}],
				[{env, [{dispatch, dispatch_rules()}]}]),
    {ok, {{one_for_one, 5, 10}, []}}.

dispatch_rules() ->
    cowboy_router:compile(
       [{'_', [
            {["/static/[...]"], cowboy_static, [{directory, {priv_dir, ?APP, [<<"static">>]}},
                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}, 
            {["/websocket/[...]"], ws_chat, []},
            {'_', nitrogen_cowboy, []}
    ]}]).
