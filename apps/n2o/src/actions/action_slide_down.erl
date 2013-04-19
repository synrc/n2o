% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2012 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_slide_down).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=slidedown,
        speed = Record#slide_down.speed,
        actions = Record#slide_down.actions
    }.
