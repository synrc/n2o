-module(element_br).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, br).

render_element(Record) -> 
    wf_tags:emit_tag(<<"br">>, [
        {<<"id">>, Record#br.id},
        {<<"class">>, Record#br.class}, 
        {<<"style">>, Record#br.style}
    ]).
