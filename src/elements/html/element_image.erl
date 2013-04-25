% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_image).
-include_lib ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, image).

render_element(Record) ->
    Attributes = [
        {id, Record#image.html_id},
        {class, [image, Record#image.class]},
        {style, Record#image.style},
        {src, Record#image.image}
    ],

    FinalAttributes = case Record#image.alt of
        undefined -> Attributes;
        ImageAlt -> [{alt, ImageAlt}|Attributes] 
    end,

    wf_tags:emit_tag(img, FinalAttributes).
