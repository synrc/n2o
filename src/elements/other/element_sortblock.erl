% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sortblock).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, sortblock).

render_element(Record) -> 
    % Get properties...
    Anchor = Record#sortblock.anchor,
    Tag = Record#sortblock.tag,
    Delegate = Record#sortblock.delegate,
    PostbackInfo = wf_event:serialize_event_context({Delegate, Tag}, Anchor, undefined, ?MODULE),
    Handle = case Record#sortblock.handle of
        undefined -> "null";
        Other -> wf:f("'.~s'", [Other])
    end,
    ConnectWithGroups = groups_to_connect_with(Record#sortblock.connect_with_groups),
    GroupClasses = groups_to_classes(Record#sortblock.group),
    Placeholder = Record#sortblock.placeholder,
    ForcePlaceholderSize = Record#sortblock.force_placeholder_size,

    % Emit the javascript...
    Script = #script { 
        script=wf:f("Nitrogen.$sortblock('~s', {handle: ~s, connectWith: [~s], placeholder: '~s', forcePlaceholderSize: ~s}, '~s');", [Anchor, Handle, ConnectWithGroups, Placeholder, ForcePlaceholderSize, PostbackInfo])
    },
    wf:wire(Script),

    element_panel:render_element(#panel {
        html_id=Record#sortblock.html_id,
        id=Record#sortblock.id,
        anchor=Record#sortblock.anchor,
        class=[sortblock, GroupClasses|Record#sortblock.class],
        style=Record#sortblock.style,
        body=Record#sortblock.items
    }).

event({Delegate, BlockTag}) ->
    SortItems = wf:q(sort_items),
    SortTags = [wf:depickle(X) || X <- string:tokens(SortItems, ",")],
    Module = wf:coalesce([Delegate, wf:page_module()]),
    Module:sort_event(BlockTag, SortTags).

groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
    Groups1 = lists:flatten([Groups]),
    Groups2 = ["drag_group_" ++ wf:to_list(X) || X <- Groups1],
    string:join(Groups2, " ").

groups_to_connect_with(all) -> "'*'";
groups_to_connect_with(undefined) -> "'*'";
groups_to_connect_with(none) -> "";
groups_to_connect_with([]) -> "'*'";
groups_to_connect_with(Groups) ->
    Groups1 = lists:flatten([Groups]),
    Groups2 = ["'.drag_group_" ++ wf:to_list(X) ++ "'" || X <- Groups1],
    string:join(Groups2, ", ").
