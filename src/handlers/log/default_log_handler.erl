% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_log_handler).
-behaviour (log_handler).
-export ([
    init/2, 
    finish/2,
    info/3, 
    warning/3, 
    error/3
]).

init(_Config, State) -> 
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

info(S, _Config, State) -> 
    error_logger:info_msg([S, "\n"]),
    {ok, State}.

warning(S, _Config, State) -> 
    error_logger:warning_msg([S, "\n"]),
    {ok, State}.

error(S, _Config, State) -> 
    error_logger:error_msg([S, "\n"]),
    {ok, State}.
