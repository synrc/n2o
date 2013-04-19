% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (log_handler).

-compile({no_auto_import,[error/1]}).

-export ([
    behaviour_info/1,
    info/1, info/2,
    warning/1, warning/2,
    error/1, error/2
]).



% info(String, State) -> {ok, NewState}.
% Log an info-level message. Everything is functioning as usual.
info(String, Args) -> 
    ok = info(wf:f(String, Args)).

info(String) -> 
    ok = wf_handler:call(log_handler, info, [String]).

% warning(String, State) -> {ok, NewState}.
% Log a warning-level message. If something is not corrected, then
% service could be interrupted in some way.
warning(String, Args) -> 
    ok = warning(wf:f(String, Args)).

warning(String) -> 
    ok = wf_handler:call(log_handler, warning, [String]).

% error(String, State) -> {ok, NewState}.
% Log an error-level message. Service has been interrupted in some way.
error(String, Args) -> 
    ok = error(wf:f(String, Args)).

error(String) -> 
    ok = wf_handler:call(log_handler, error, [String]).



behaviour_info(callbacks) -> [
    {init, 2},      
    {finish, 2},
    {info, 3},       
    {warning, 3},	
    {error, 3}
];
behaviour_info(_) -> undefined.
