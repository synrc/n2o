-module(action_remove_class).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=remove_class,
        class = Record#remove_class.class,
        speed = Record#remove_class.speed,
        actions = Record#remove_class.actions
    }.
