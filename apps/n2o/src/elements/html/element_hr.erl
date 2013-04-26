-module(element_hr).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

reflect() -> record_info(fields, hr).

render_element(Record) -> 
    wf_tags:emit_tag(hr, [
        {id, Record#hr.id},
        {size, 1},
        {class, [hr, Record#hr.class]},
        {style, Record#hr.style}
    ]).
