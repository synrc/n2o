% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_droppable).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, droppable).

render_element(Record) -> 
    % Get properties...
    Delegate = Record#droppable.delegate,
    Tag = Record#droppable.tag,
    Anchor = Record#droppable.anchor,
    PostbackInfo = wf_event:serialize_event_context({Delegate, Tag}, Anchor, undefined, ?MODULE),
    ActiveClass = Record#droppable.active_class, 
    HoverClass = Record#droppable.hover_class,
    AcceptGroups = groups_to_accept(Record#droppable.accept_groups),

    % Write out the script to make this element droppable...
    Script = #script {
        script=wf:f("Nitrogen.$droppable('~s', { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');", [Anchor, ActiveClass, HoverClass, AcceptGroups, PostbackInfo])
    },
    wf:wire(Script),

    % Render as a panel.
    element_panel:render_element(#panel {
        id=Record#droppable.id,
        anchor=Record#droppable.anchor,
        class=[droppable, Record#droppable.class],
        style=Record#droppable.style,
        body=Record#droppable.body
    }).

event({Delegate, DropTag}) ->
    DragItem = wf:q(drag_item),
    DragTag = wf:depickle(DragItem),
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Module:drop_event(DragTag, DropTag).

groups_to_accept(all) -> "*";
groups_to_accept(undefined) -> "*";
groups_to_accept(none) -> "";
groups_to_accept([]) -> "*";
groups_to_accept(Groups) ->
    Groups1 = lists:flatten([Groups]),
    Groups2 = [".drag_group_" ++ wf:to_list(X) || X <- Groups1],
    string:join(Groups2, ", ").
