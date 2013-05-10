-module(element_em).
-author('Sergei Lebedev').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

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
