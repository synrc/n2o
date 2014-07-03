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
    {<<"alt">>, Record#image.alt},
    {<<"width">>, Record#image.width},
    {<<"height">>, Record#image.height},
    {<<"src">>, wf:coalesce([Record#image.src, Record#image.image])} | Record#image.data_fields
  ],

  wf_tags:emit_tag(<<"img">>, Attributes).
