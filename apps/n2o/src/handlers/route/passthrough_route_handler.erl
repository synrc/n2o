% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (passthrough_route_handler).
-behaviour (route_handler).
-include_lib ("wf.hrl").
-export ([
    init/2, 
    finish/2
]).

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
