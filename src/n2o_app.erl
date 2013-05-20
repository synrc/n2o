-module(n2o_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> n2o_sup:start_link().
stop(_State) -> ok.
