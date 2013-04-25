% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_security_handler).
-behaviour (security_handler).
-export ([
    init/2, 
    finish/2
]).


init(_Config, State) -> 
    % By default, let all requests through. If we wanted to impose
    % security, then check the page module (via wf:page_module()),
    % and if the user doesn't have access, then set a new page module and path info,
    % via wf_context:page_module(Module), wf_context:path_info(PathInfo).
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.
