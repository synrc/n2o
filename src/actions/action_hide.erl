-module(action_hide).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=hide,
        effect = Record#hide.effect,
        options = Record#hide.options,
        speed = Record#hide.speed,
        actions = Record#hide.actions
    }.
