-module(n2o_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib ("n2o/include/wf.hrl").

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    ets:new(cookies,[set,named_table,{keypos,1},public]),
    ets:new(actions,[set,named_table,{keypos,1},public]),
    ets:new(globals,[set,named_table,{keypos,1},public]),
    ets:new(caching,[set,named_table,{keypos,1},public]),
    ets:insert(globals,{onlineusers,0}),
    
    {ok, {{one_for_one, 5, 10}, []}}.

