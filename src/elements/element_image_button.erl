-module(element_image_button).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, image_button).

render_element(Record) ->.
    List = [
      {<<"id">>, Record#image_button.id},
      {<<"type">>, <<"image">>},
      {<<"style">>,Record#image_button.style},
      {<<"name">>,Record#image_button.name},
      {<<"accesskey">>, Record#image_button.accesskey},
      {<<"value">>, Record#image_button.value},
      {<<"alt">>, Record#image_button.alt},
      {<<"height">>, Record#image_button.height},
      {<<"width">>, Record#image_button.width},
      {<<"src">>, Record#image_button.src},
      {<<"class">>,Record#image_button.class} | Record#image_button.data_fields
  ] ++ case Record#image_button.disabled of undefined -> []; _ -> [{<<"disabled">>,<<"disabled">>}] end,
  wf_tags:emit_tag(<<"input">>, wf:render(Record#image_button.body), List).