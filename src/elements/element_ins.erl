-module(element_ins).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, ins).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#ins.accesskey},
      {<<"class">>, Record#ins.class},
      {<<"contenteditable">>, case Record#ins.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#ins.contextmenu},
      {<<"dir">>, case Record#ins.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#ins.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#ins.dropzone},
      {<<"hidden">>, case Record#ins.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#ins.id},
      {<<"lang">>, Record#ins.lang},
      {<<"spellcheck">>, case Record#ins.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#ins.style},
      {<<"tabindex">>, Record#ins.tabindex},
      {<<"title">>, Record#ins.title},
      {<<"translate">>, case Record#ins.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"cite">>, Record#ins.cite},
      {<<"datetime">>, Record#ins.datetime} | Record#ins.data_fields
    ],
    wf_tags:emit_tag(<<"ins">>, wf:render(case Record#ins.body of undefined -> []; B -> B end), List).