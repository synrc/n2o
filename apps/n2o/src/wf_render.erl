% Copyright (c) 2008-2010 Rusty Klophaus

-module (wf_render).
-include_lib ("wf.hrl").
-export ([
    render/5
]).

render(Elements, Actions, Anchor, Trigger, Target) ->
    % Save any queued actions...
    OldQueuedActions = wf_context:actions(),
    wf_context:clear_actions(),

    % First, render the elements.
    {ok, Html} = wf_render_elements:render_elements(Elements),

    % Second, render any actions.
    {ok, Script1} = wf_render_actions:render_actions(Actions, Anchor, Trigger, Target),

    % Third, render queued actions that were a result of step 1 or 2.
    QueuedActions = wf_context:actions(),
    {ok, Script2} = wf_render_actions:render_actions(QueuedActions, Anchor, Trigger, Target),

    % Restore queued actions...
    wf_context:actions(OldQueuedActions),

    % Return.
    Script=[Script1, Script2],
    {ok, Html, Script}.


