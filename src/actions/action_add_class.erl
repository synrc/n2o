-module(action_add_class).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    #jquery_effect {
        type=add_class,
        class = Record#add_class.class,
        speed = Record#add_class.speed,
        actions = Record#add_class.actions
    }.
