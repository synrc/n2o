-module(element_output).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, output).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#output.accesskey},
      {<<"class">>, Record#output.class},
      {<<"contenteditable">>, case Record#output.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#output.contextmenu},
      {<<"dir">>, case Record#output.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#output.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#output.dropzone},
      {<<"hidden">>, case Record#output.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#output.id},
      {<<"lang">>, Record#output.lang},
      {<<"spellcheck">>, case Record#output.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#output.style},
      {<<"tabindex">>, Record#output.tabindex},
      {<<"title">>, Record#output.title},
      {<<"translate">>, case Record#output.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"for">>,Record#output.for},
      {<<"form">>,Record#output.form},
      {<<"name">>,Record#output.name} | Record#output.data_fields
    ],
    wf_tags:emit_tag(<<"output">>, wf:render(case Record#output.body of undefined -> []; B -> B end), List).