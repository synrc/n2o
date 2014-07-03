-module(element_canvas).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, canvas).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#canvas.accesskey},
      {<<"class">>, Record#canvas.class},
      {<<"contenteditable">>, case Record#canvas.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#canvas.contextmenu},
      {<<"dir">>, case Record#canvas.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#canvas.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#canvas.dropzone},
      {<<"hidden">>, case Record#canvas.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#canvas.id},
      {<<"lang">>, Record#canvas.lang},
      {<<"spellcheck">>, case Record#canvas.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#canvas.style},
      {<<"tabindex">>, Record#canvas.tabindex},
      {<<"title">>, Record#canvas.title},
      {<<"translate">>, case Record#canvas.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"height">>,Record#canvas.height},      
      {<<"width">>,Record#canvas.width} | Record#canvas.data_fields
    ],
    wf_tags:emit_tag(<<"canvas">>, wf:render(case Record#canvas.body of undefined -> []; B -> B end), List).