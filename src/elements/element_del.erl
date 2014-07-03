-module(element_del).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, del).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#del.accesskey},
      {<<"class">>, Record#del.class},
      {<<"contenteditable">>, case Record#del.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#del.contextmenu},
      {<<"dir">>, case Record#del.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#del.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#del.dropzone},
      {<<"hidden">>, case Record#del.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#del.id},
      {<<"lang">>, Record#del.lang},
      {<<"spellcheck">>, case Record#del.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#del.style},
      {<<"tabindex">>, Record#del.tabindex},
      {<<"title">>, Record#del.title},
      {<<"translate">>, case Record#del.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"cite">>, Record#del.cite},
      {<<"datetime">>, Record#del.datetime} | Record#del.data_fields
    ],
    wf_tags:emit_tag(<<"del">>, wf:render(case Record#del.body of undefined -> []; B -> B end), List).