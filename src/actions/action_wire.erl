-module(action_wire).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

% We have set of events that we wire them

render_action(#wire{actions=Api=#api{}}) -> action_api:render_action(Api);
render_action(#wire{actions=Redirect=#redirect{}}) -> action_redirect:render_action(Redirect);
render_action(#wire{actions=Event=#event{}}) -> action_event:render_action(Event);
render_action(#wire{actions=Script=#script{}}) -> action_script:render_action(Script);
render_action(#wire{actions=Actions}) when is_list(Actions) -> Actions;
render_action(S) when is_list(S) -> S;
render_action(_) -> [].

wire(_, _, undefined) -> ok;
wire(_, _, []) -> ok;
wire(Trigger, Target, Actions) when is_binary(Actions) orelse ?IS_STRING(Actions) ->
    wire(Trigger, Target, #script { script=Actions });
wire(Trigger, Target, Actions) -> wf_context:add_action(#wire{ trigger=Trigger, target=Target, actions=Actions }).
