-module(action_appear).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=appear,
        speed = Record#appear.speed,
        actions = Record#appear.actions
    }.
