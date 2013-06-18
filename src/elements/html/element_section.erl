-module(element_section).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, section).

render_element(Record) ->
    wf_tags:emit_tag(<<"section">>, wf:render(Record#section.body), [
        {<<"id">>, Record#section.id},
        {<<"class">>, Record#section.class},
        {<<"style">>, Record#section.style}
    ]).
