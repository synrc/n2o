-module(element_section).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, section).

render_element(Record) ->
    wf_tags:emit_tag(section, Record#section.body, [
        {id, Record#section.id},
        {class, ["section", Record#section.class]},
        {style, Record#section.style}
    ]).
