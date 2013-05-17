-module(action_wire).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#wire{actions=Api=#api{}}) -> action_api:render_action(Api);
render_action(#wire{actions=Event=#event{}}) -> action_event:render_action(Event);
render_action(#wire{actions=Script=#script{}}) -> action_script:render_action(Script);
render_action(#wire{actions=Actions}) when is_list(Actions) -> Actions;
%render_action(#wire{actions=Actions}) when is_tuple(Actions) -> wf_render_actions:render_action(Actions);
%render_action(#wire{actions=Actions}) when is_list(Actions) -> wf_render_actions:render_actions(Actions);
render_action(S) when is_list(S) -> S;
render_action(_) -> [].

wire(_, _, undefined) -> ok;
wire(_, _, []) -> ok;
wire(Trigger, Target, Actions) when is_binary(Actions) orelse ?IS_STRING(Actions) ->
    wire(Trigger, Target, #script { script=Actions });
wire(Trigger, Target, Actions) -> wf_context:add_action(#wire{ trigger=Trigger, target=Target, actions=Actions }).
