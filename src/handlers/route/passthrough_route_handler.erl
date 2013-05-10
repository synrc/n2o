-module(passthrough_route_handler).
-author('Rusty Klophaus').
-behaviour(route_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

init(Module, State) -> 
    % Some values...
    RequestBridge = wf_context:request_bridge(),
    Path = RequestBridge:path(),

    % Update the page_context with the path and module.
    wf_context:page_module(Module),
    wf_context:path_info(Path),
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.
