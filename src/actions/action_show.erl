-module(action_show).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=show,
        effect = Record#show.effect,
        options = Record#show.options,
        speed = Record#show.speed,
        actions = Record#show.actions
    }.
