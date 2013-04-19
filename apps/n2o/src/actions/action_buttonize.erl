% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_buttonize).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(_Record) -> 
    Actions = [
        #event { type=mouseover, actions=#add_class { class=hover } },
        #event { type=mouseout, actions=#remove_class { class=hover } },
        #event { type=mousedown, actions=#add_class { class=clicked } },
        #event { type=mouseup, actions=#remove_class { class=clicked } }
    ],
    Actions.
