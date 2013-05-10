-module(action_effect).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=effect,
        target = Record#effect.target,
        effect = Record#effect.effect,
        options = Record#effect.options,
        speed = Record#effect.speed,
        actions = Record#effect.actions
    }.
