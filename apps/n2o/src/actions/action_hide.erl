% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_hide).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=hide,
        effect = Record#hide.effect,
        options = Record#hide.options,
        speed = Record#hide.speed,
        actions = Record#hide.actions
    }.
