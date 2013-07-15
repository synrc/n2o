-module(element_placeholder).
-author('Rusty Klophaus').
-include("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, placeholder).

render_element(Record) -> 
    Record#placeholder.body.
