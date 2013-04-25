% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (element_time).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, time).

render_element(Record) ->
    case {Record#time.pubdate, Record#time.datetime} of
        {false, ""} ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.html_id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style}
            ]);
        {true, ""}  ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.html_id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style},
                {pubdate}
            ]);
        {false, _}  ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.html_id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style},
                {datetime, Record#time.datetime}
            ]);
        {true, _}   ->
            wf_tags:emit_tag(time, Record#time.body, [
                {id, Record#time.html_id},
                {class, ["time", Record#time.class]},
                {style, Record#time.style},
                {datetime, Record#time.datetime},
                {pubdate}
            ])
    end.
