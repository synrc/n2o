-module(element_br).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

reflect() -> record_info(fields, br).

render_element(Record) -> 
    wf_tags:emit_tag(br, [
        {id, Record#br.id},
        {class, [br, Record#br.class]}, 
        {style, Record#br.style}
    ]).
