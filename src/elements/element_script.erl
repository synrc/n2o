-module(element_script).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, script).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#script.accesskey},
      {<<"class">>, Record#script.class},
      {<<"contenteditable">>, case Record#script.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#script.contextmenu},
      {<<"dir">>, case Record#script.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#script.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#script.dropzone},
      {<<"hidden">>, case Record#script.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#script.id},
      {<<"lang">>, Record#script.lang},
      {<<"spellcheck">>, case Record#script.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#script.style},
      {<<"tabindex">>, Record#script.tabindex},
      {<<"title">>, Record#script.title},
      {<<"translate">>, case Record#script.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"async">>, if Record#script.async == true -> "async"; true -> undefined end},
      {<<"charset">>,Record#script.charset},
      {<<"defer">>, if Record#script.defer == true -> "defer"; true -> undefined end},
      {<<"src">>,Record#script.src},
      {<<"type">>,Record#script.type} | Record#script.data_fields
    ],
    wf_tags:emit_tag(<<"script">>, wf:render(case Record#script.body of undefined -> []; B -> B end), List).