-module(element_meter).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, meter).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#meter.accesskey},
      {<<"class">>, Record#meter.class},
      {<<"contenteditable">>, case Record#meter.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#meter.contextmenu},
      {<<"dir">>, case Record#meter.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#meter.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#meter.dropzone},
      {<<"hidden">>, case Record#meter.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#meter.id},
      {<<"lang">>, Record#meter.lang},
      {<<"spellcheck">>, case Record#meter.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#meter.style},
      {<<"tabindex">>, Record#meter.tabindex},
      {<<"title">>, Record#meter.title},
      {<<"translate">>, case Record#meter.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"high">>,Record#meter.high},
      {<<"low">>,Record#meter.low},
      {<<"max">>,Record#meter.max},
      {<<"min">>,Record#meter.min},
      {<<"optimum">>,Record#meter.optimum},
      {<<"value">>, wf:js_escape(Record#meter.value)} | Record#meter.data_fields
    ],
    wf_tags:emit_tag(<<"meter">>, wf:render(case Record#meter.body of undefined -> []; B -> B end), List).