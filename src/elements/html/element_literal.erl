% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_literal).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, literal).

render_element(Record) -> 
    wf:html_encode(Record#literal.text, Record#literal.html_encode).
