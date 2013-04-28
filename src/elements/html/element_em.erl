% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2011 Sergei Lebedev
% See MIT-LICENSE for licensing information.

-module (element_em).
-compile(export_all).
-include_lib ("wf.hrl").

reflect() -> record_info(fields, em).

render_element(Record) ->
    Body = [
        wf:html_encode(Record#em.text, Record#em.html_encode),
        Record#em.body
    ],
    wf_tags:emit_tag(em, Body, [
        {class, [p, Record#em.class]},
        {style, Record#em.style}
    ]).
