-module(action_wire).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#wire{actions=Actions}) -> wf:render(Actions);
render_action(S) when is_list(S) -> S;
render_action(_) -> [].

wire(_, _, undefined) -> ok;
wire(_, _, []) -> ok;
wire(Trigger, Target, Actions) -> wf_context:add_action(#wire{ trigger=Trigger, target=Target, actions=Actions }).
