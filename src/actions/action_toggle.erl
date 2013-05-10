-module(action_toggle).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=toggle,
        effect = Record#toggle.effect,
        options = Record#toggle.options,
        speed = Record#toggle.speed,
        actions = Record#toggle.actions
    }.
