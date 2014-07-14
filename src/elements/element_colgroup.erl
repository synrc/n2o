-module(element_colgroup).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, colgroup).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#colgroup.accesskey},
      {<<"class">>, Record#colgroup.class},
      {<<"contenteditable">>, case Record#colgroup.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#colgroup.contextmenu},
      {<<"dir">>, case Record#colgroup.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#colgroup.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#colgroup.dropzone},
      {<<"hidden">>, case Record#colgroup.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#colgroup.id},
      {<<"lang">>, Record#colgroup.lang},
      {<<"spellcheck">>, case Record#colgroup.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#colgroup.style},
      {<<"tabindex">>, Record#colgroup.tabindex},
      {<<"title">>, Record#colgroup.title},
      {<<"translate">>, case Record#colgroup.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"span">>,Record#colgroup.span} | Record#colgroup.data_fields
    ],
    wf_tags:emit_tag(<<"colgroup">>, wf:render(case Record#colgroup.body of undefined -> []; B -> B end), List).