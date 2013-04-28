% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_remove_class).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=remove_class,
        class = Record#remove_class.class,
        speed = Record#remove_class.speed,
        actions = Record#remove_class.actions
    }.
