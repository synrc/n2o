% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_effect).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=effect,
        anchor = Record#effect.anchor,
        target = Record#effect.target,
        effect = Record#effect.effect,
        options = Record#effect.options,
        speed = Record#effect.speed,
        actions = Record#effect.actions
    }.
