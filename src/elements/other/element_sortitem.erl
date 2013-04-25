% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sortitem).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, sortitem).

render_element(Record) -> 
    Anchor = Record#sortitem.anchor,
    PickledTag = wf:pickle(Record#sortitem.tag),
    Script = #script { 
        script=wf:f("Nitrogen.$sortitem('~s', '~s');", [Anchor, PickledTag])
    },
    wf:wire(Script),

    Panel = #panel {
        html_id=Record#sortitem.html_id,
        id=Record#sortitem.id,
        anchor=Record#sortitem.anchor,
        class=[sortitem, Record#sortitem.class],
        style=Record#sortitem.style,
        body=Record#sortitem.body
    },

    element_panel:render_element(Panel).

