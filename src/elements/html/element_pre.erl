% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2011 Sergei Lebedev
% See MIT-LICENSE for licensing information.

-module (element_pre).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, pre).

render_element(Record) ->
    Body = wf:html_encode(Record#pre.text, Record#pre.html_encode),
    wf_tags:emit_tag(pre, Body, [
        {class, [pre, Record#pre.class]},
        {style, Record#pre.style}
    ]).
