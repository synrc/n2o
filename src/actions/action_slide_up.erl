-module(action_slide_up).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=slideup,
        speed = Record#slide_up.speed,
        actions = Record#slide_up.actions
    }.
