-module(element_mark).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, mark).

render_element(Record) ->
    wf_tags:emit_tag(mark, Record#mark.body, [
        {id, Record#mark.id},
        {class, ["mark", Record#mark.class]},
        {style, Record#mark.style}
    ]).
