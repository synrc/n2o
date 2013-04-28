% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_function).
-include_lib ("wf.hrl").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record) ->
    F = Record#function.function,
    F().
