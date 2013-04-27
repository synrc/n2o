-module(element_range).
-author('Andrew Zadorozhny').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, range).

render_element(Record) -> 
    ID = Record#range.id,

    wf_tags:emit_tag(input, [
        {type, range}, 
        {class, [range, Record#range.class]},
        {min, Record#range.min},
        {max, Record#range.max},
        {step, Record#range.step},
        {style, Record#range.style},
        {id, Record#range.id},
        {value, Record#range.value},
        {data_fields, Record#range.data_fields}
    ]).
