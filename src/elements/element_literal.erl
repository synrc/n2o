-module(element_literal).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, literal).
render_element(Record) -> wf:html_encode(Record#literal.body).
