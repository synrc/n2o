-module(element_nav).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, nav).

render_element(Record) ->
    wf_tags:emit_tag(nav, Record#nav.body, [
        {id, Record#nav.id},
        {class, ["nav", Record#nav.class]},
        {style, Record#nav.style}
    ]).
