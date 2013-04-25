-module(action_wire).
-author('Andrew Zadorozhny').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    DefaultAnchor = Record#wire.anchor,
    DefaultTrigger = Record#wire.trigger,
    DefaultTarget = Record#wire.target,
    Actions = set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, Record#wire.actions),
    error_logger:info_msg("After Set Path ~p~n",[1]), 
    Actions.

set_paths(_DefaultAnchor, _DefaultTrigger, _DefaultTarget, []) -> [];
set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, [H|T]) ->
    [ set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, H)|
      set_paths(DefaultAnchor, DefaultTarget, DefaultTarget, T) ];

set_paths(DefaultAnchor, DefaultTrigger, DefaultTarget, Action) when is_tuple(Action) ->
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

wire(_, _, undefined) -> ok;
wire(_, _, []) -> ok;

wire(Trigger, Target, Actions) when is_binary(Actions) orelse ?IS_STRING(Actions) ->
    wire(Trigger, Target, #script { script=Actions });

wire(Trigger, Target, Actions) -> wf_context:add_action(#wire{ trigger=Trigger, target=Target, actions=Actions }).
