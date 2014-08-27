-module(element_map).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, map).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#map.accesskey},
      {<<"class">>, Record#map.class},
      {<<"contenteditable">>, case Record#map.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#map.contextmenu},
      {<<"dir">>, case Record#map.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#map.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#map.dropzone},
      {<<"hidden">>, case Record#map.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#map.id},
      {<<"lang">>, Record#map.lang},
      {<<"spellcheck">>, case Record#map.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#map.style},
      {<<"tabindex">>, Record#map.tabindex},
      {<<"title">>, Record#map.title},
      {<<"translate">>, case Record#map.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"name">>,Record#map.name} | Record#map.data_fields
    ],
    wf_tags:emit_tag(<<"map">>, wf:render(case Record#map.body of undefined -> []; B -> B end), List).