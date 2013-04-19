% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_tablecell).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tablecell).

render_element(Record) -> 
    Body = [
        wf:html_encode(Record#tablecell.text, Record#tablecell.html_encode),
        Record#tablecell.body
    ],

    wf_tags:emit_tag(td, Body, [
        {id, Record#tablecell.html_id},
        {class, [tablecell, Record#tablecell.class]},
        {style, Record#tablecell.style},
        {align, Record#tablecell.align},
        {valign, Record#tablecell.valign},
        {colspan, Record#tablecell.colspan},
        {rowspan, Record#tablecell.rowspan}	
    ]).
