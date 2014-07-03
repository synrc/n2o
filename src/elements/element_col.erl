-module(element_col).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, col).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#col.accesskey},
      {<<"class">>, Record#col.class},
      {<<"contenteditable">>, case Record#col.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#col.contextmenu},
      {<<"dir">>, case Record#col.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#col.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#col.dropzone},
      {<<"hidden">>, case Record#col.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#col.id},
      {<<"lang">>, Record#col.lang},
      {<<"spellcheck">>, case Record#col.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#col.style},
      {<<"tabindex">>, Record#col.tabindex},
      {<<"title">>, Record#col.title},
      {<<"translate">>, case Record#col.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"span">>,Record#col.span} | Record#col.data_fields
    ],
    wf_tags:emit_tag(<<"col">>, wf:render(case Record#col.body of undefined -> []; B -> B end), List).