% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Steffen Panning
% See MIT-LICENSE for licensing information.

-module (element_restful_submit).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, restful_submit).

render_element(Record) ->
    Value = ["  ", wf:html_encode(Record#restful_submit.text, Record#restful_submit.html_encode), "  "], 
    wf_tags:emit_tag(input, [
        {type,  submit},
	{name, Record#restful_submit.html_name},
        {class, [restful_submit, Record#restful_submit.class]},
        {style, Record#restful_submit.style},
        {value, Value}
    ]).
