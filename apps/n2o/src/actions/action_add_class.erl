% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_add_class).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=add_class,
        class = Record#add_class.class,
        speed = Record#add_class.speed,
        actions = Record#add_class.actions
    }.
