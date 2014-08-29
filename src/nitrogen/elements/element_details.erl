-module(element_details).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, details).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#details.accesskey},
      {<<"class">>, Record#details.class},
      {<<"contenteditable">>, case Record#details.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#details.contextmenu},
      {<<"dir">>, case Record#details.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#details.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#details.dropzone},
      {<<"hidden">>, case Record#details.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#details.id},
      {<<"lang">>, Record#details.lang},
      {<<"spellcheck">>, case Record#details.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#details.style},
      {<<"tabindex">>, Record#details.tabindex},
      {<<"title">>, Record#details.title},
      {<<"translate">>, case Record#details.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"open">>, case Record#details.open of true -> "open"; _ -> undefined end} | Record#details.data_fields
    ],
    wf_tags:emit_tag(<<"details">>, wf:render(case Record#details.body of undefined -> []; B -> B end), List).