% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2011 Sergei Lebedev
% See MIT-LICENSE for licensing information.

-module (element_strong).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, strong).

render_element(Record) ->
    Body = [
        wf:html_encode(Record#strong.text, Record#strong.html_encode),
        Record#strong.body
    ],
    wf_tags:emit_tag(strong, Body, [
        {class, [p, Record#strong.class]},
        {style, Record#strong.style}
    ]).
