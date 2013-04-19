%$ vim: ts=4 sw=4 et
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_range).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, range).

render_element(Record) -> 
    ID = Record#range.id,
    Anchor = Record#range.anchor,
    case Record#range.next of
        undefined -> ignore;
        Next -> 
            Next1 = wf_render_actions:normalize_path(Next),
            wf:wire(Anchor, #event { type=enterkey, actions=wf:f("Nitrogen.$go_next('~s');", [Next1]) })
    end,

    case Record#range.postback of
        undefined -> ignore;
        Postback -> wf:wire(Anchor, #event { type=change, postback=Postback, validation_group=ID, delegate=Record#range.delegate })
    end,

    wf_tags:emit_tag(input, [
        {type, range}, 
        {class, [range, Record#range.class]},
        {min, Record#range.min},
        {max, Record#range.max},
        {step, Record#range.step},
        {style, Record#range.style},
        {id, Record#range.html_id},
        {value, Record#range.value},
        {data_fields, Record#range.data_fields}
    ]).
