-module(action_wire).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#wire{actions=Actions}) -> wf:render(Actions);
render_action(S) when is_list(S) -> S;
render_action(_) -> [].

wire(Actions) -> wf:add_action(#wire{actions=Actions}).
