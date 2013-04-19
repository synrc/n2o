% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_update).
-include_lib ("wf.hrl").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record) ->
    Type    = Record#update.type,
    Anchor  = Record#update.anchor,
    Trigger = Record#update.trigger,
    Target  = Record#update.target,

    % Render into HTML and Javascript...
    Elements = Record#update.elements,
    {ok, Html, Script} = wf_render:render(Elements, [], Anchor, Trigger, Target), 

    % Turn the HTML into a Javascript statement that will update the right element.
    ScriptifiedHtml = wf:f("Nitrogen.$~s(\"~s\", \"~s\", \"~s\");", [Type, Anchor, Target, wf:js_escape(Html)]),
    [ScriptifiedHtml, Script].

update(Target, Elements) -> 
    update(update, Target, Elements).

replace(Target, Elements) ->
    update(replace, Target, Elements).

insert_top(Target, Elements) -> 
    update(insert_top, Target, Elements).

insert_bottom(Target, Elements) -> 
    update(insert_bottom, Target, Elements).

insert_before(Target, Elements) ->
    update(insert_before, Target, Elements).

insert_after(Target, Elements) ->
    update(insert_after, Target, Elements).

remove(Target) ->
    update(remove, Target, []).

%%% PRIVATE FUNCTIONS %%%

update(Type, Target, Elements) ->
    Anchor = wf_context:anchor(),
    Action = #update {
        type=Type,
        anchor  = Anchor, 
        target  = wf:coalesce([Target, Anchor]), 
        elements=Elements		
    },

    wf_context:add_action([Action]),
    ok.
