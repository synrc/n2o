-module(wf_render).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render(Elements, Actions, Trigger, Target) ->
    OldQueuedActions = wf_context:actions(),
    wf_context:clear_actions(),
    {ok, Html} = wf_render_elements:render_elements(Elements),
    {ok, Script1} = wf_render_actions:render_actions(Actions, Trigger, Target),
    QueuedActions = wf_context:actions(),
    {ok, Script2} = wf_render_actions:render_actions(QueuedActions, Trigger, Target),
    wf_context:actions(OldQueuedActions),
    Script=[Script1, Script2],
    {ok, Html, Script}.
