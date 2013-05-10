-module(element_time).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, time).

render_element(Record) ->
    case {Record#time.pubdate, Record#time.datetime} of
        {false, ""} ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style}
            ]);
        {true, ""}  ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style},
                {pubdate}
            ]);
        {false, _}  ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style},
                {datetime, Record#time.datetime}
            ]);
        {true, _}   ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style},
                {datetime, Record#time.datetime},
                {pubdate}
            ])
    end.
