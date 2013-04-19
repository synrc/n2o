% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2012 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_slide_up).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=slideup,
        speed = Record#slide_up.speed,
        actions = Record#slide_up.actions
    }.
