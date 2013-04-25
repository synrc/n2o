% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_placeholder).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, placeholder).

render_element(Record) -> 
    Record#placeholder.body.
