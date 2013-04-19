% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_wire).
-include_lib ("wf.hrl").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record) ->
    try 
        DefaultAnchor = Record#wire.anchor,
    	DefaultTrigger = Record#wire.trigger,
    	DefaultTarget = Record#wire.target,
    	Actions = set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, Record#wire.actions),
    	[Actions]
    catch Type : Error ->
        ?PRINT(Type),
        ?PRINT(Error),
        ?PRINT(Record),
        erlang:Type(Error)
    end.

set_paths(_DefaultAnchor, _DefaultTrigger, _DefaultTarget, []) -> 
    [];

set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, [H|T]) ->
    [set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, H)|
        set_paths(DefaultAnchor, DefaultTarget, DefaultTarget, T)];

set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, Action) when is_tuple(Action) ->
    % If the action doesn't have a target
    Anchor  = wf:coalesce([get_anchor(Action), DefaultAnchor]),
    Action1 = set_anchor(Action, Anchor),

    Trigger = wf:coalesce([get_trigger(Action1), DefaultTrigger]),
    Action2 = set_trigger(Action1, Trigger),

    Target = wf:coalesce([get_target(Action2), DefaultTarget]),
    set_target(Action2, Target);

set_paths(_, _, _, Other) -> Other.

get_anchor(Action) -> element(4, Action).
set_anchor(Action, Anchor) -> setelement(4, Action, Anchor).
get_trigger(Action) -> element(5, Action).
set_trigger(Action, Trigger) -> setelement(5, Action, Trigger).
get_target(Action) -> element(6, Action).
set_target(Action, Target) -> setelement(6, Action, Target).

wire(_, _, undefined) -> 
    ok;

wire(_, _, []) -> 
    ok;

wire(Trigger, Target, Actions) when is_binary(Actions) orelse ?IS_STRING(Actions) ->
    wire(Trigger, Target, #script { script=Actions });

wire(Trigger, Target, Actions) ->
    Anchor = wf_context:anchor(),
    Action = #wire {
		%% TODO: Add defer property then the actions should be sorted before being rendered
        anchor  = Anchor, 
        trigger = wf:coalesce([Trigger, Anchor]), 
        target  = wf:coalesce([Target, Anchor]), 
        actions = Actions
    },
    wf_context:add_action(Action),
    ok.
