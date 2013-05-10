-module(action_fade).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=fade,
        speed = Record#fade.speed,
        actions = Record#fade.actions
    }.
