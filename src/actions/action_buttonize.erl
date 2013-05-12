-module(action_buttonize).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(_Record) -> 
    Actions = [
        #event { type=mouseover, actions=#add_class { class=hover } },
        #event { type=mouseout, actions=#remove_class { class=hover } },
        #event { type=mousedown, actions=#add_class { class=clicked } },
        #event { type=mouseup, actions=#remove_class { class=clicked } }
    ],
    Actions.
