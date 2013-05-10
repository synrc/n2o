-module(log_handler).
-author('Rusty Klophaus').
-compile({no_auto_import,[error/1]}).
-export([behaviour_info/1, info/1, info/2, warning/1, warning/2, error/1, error/2]).

% info(String, State) -> {ok, NewState}.
% Log an info-level message. Everything is functioning as usual.
info(String, Args) -> info(wf:f(String, Args)).
info(String) -> wf_handler:call(log_handler, info, [String]).

% warning(String, State) -> {ok, NewState}.
% Log a warning-level message. If something is not corrected, then
% service could be interrupted in some way.
warning(String, Args) -> warning(wf:f(String, Args)).
warning(String) -> wf_handler:call(log_handler, warning, [String]).

% error(String, State) -> {ok, NewState}.
% Log an error-level message. Service has been interrupted in some way.
error(String, Args) -> error(wf:f(String, Args)).
error(String) -> wf_handler:call(log_handler, error, [String]).

behaviour_info(callbacks) -> [
    {init, 2},      
    {finish, 2},
    {info, 3},       
    {warning, 3},	
    {error, 3}
];
behaviour_info(_) -> undefined.
