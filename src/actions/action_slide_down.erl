-module(action_slide_down).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=slidedown,
        speed = Record#slide_down.speed,
        actions = Record#slide_down.actions
    }.
