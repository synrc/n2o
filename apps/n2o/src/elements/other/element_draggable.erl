% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_draggable).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, draggable).

render_element(Record) -> 
    % Get properties...
    Anchor = Record#draggable.anchor,
    PickledTag = wf:pickle(Record#draggable.tag),	
    GroupClasses = groups_to_classes(Record#draggable.group),

    Handle = case Record#draggable.handle of
        undefined -> "null";
        Other2 -> wf:f("'.~s'", [Other2])
    end,

    Helper = case Record#draggable.clone of
        true -> clone;
        false -> original
    end,

    Revert = case Record#draggable.revert of
        true -> "true";
        false -> "false";
        valid -> "'valid'";
        invalid -> "'invalid'"
    end,

    Container = case Record#draggable.container of
                    false -> false;
                    window -> "'window'";
                    parent -> "'parent'";
                    document -> "'document'";
                    V -> V
                end,
    
    % Write out the script to make this element draggable...
    Script = wf:f(
               "Nitrogen.$draggable('~s', { handle: ~s, helper: '~s', revert: ~s, scroll: ~s, containment: ~s, zIndex: ~p, appendTo: 'body' }, '~s');",
               [
                Anchor,
                Handle,
                Helper, 
                Revert, 
                Record#draggable.scroll,
                Container,
		Record#draggable.zindex,
		PickledTag
               ]),
    wf:wire(Script),

    % Render as a panel...
    element_panel:render_element(#panel {
        id=Record#draggable.id,
        anchor=Anchor,
        class=[draggable, GroupClasses, Record#draggable.class],
        style=Record#draggable.style,
        body=Record#draggable.body
    }).

groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
    Groups1 = lists:flatten([Groups]),
    Groups2 = ["drag_group_" ++ wf:to_list(X) || X <- Groups1],
    string:join(Groups2, " ").

