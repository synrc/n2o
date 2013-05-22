-module(element_p).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, p).

render_element(Record) -> 
    Inner = wf:render(Record#p.body),
    Body = [
        Record#p.text,
        Inner
    ],
    wf_tags:emit_tag(<<"p">>, Body, [
        {<<"id">>, Record#p.id},
        {<<"class">>, Record#p.class},
        {<<"style">>, Record#p.style}
    ]).

