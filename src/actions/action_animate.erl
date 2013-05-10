-module(action_animate).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=animate,
        options = Record#animate.options,
        speed = Record#animate.speed,
        easing = Record#animate.easing,
        actions = Record#animate.actions
    }.
