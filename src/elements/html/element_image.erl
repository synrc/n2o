-module(element_image).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, image).

render_element(Record) ->
    Attributes = [
        {<<"id">>, Record#image.id},
        {<<"class">>, Record#image.class},
        {<<"style">>, Record#image.style},
        {<<"src">>, Record#image.image}
    ],

    FinalAttributes = case Record#image.alt of
        undefined -> Attributes;
        ImageAlt -> [{alt, ImageAlt}|Attributes] 
    end,

    wf_tags:emit_tag(img, FinalAttributes).
