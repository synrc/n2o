-module(element_style).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, style).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#style.accesskey},
      {<<"class">>, Record#style.class},
      {<<"contenteditable">>, case Record#style.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#style.contextmenu},
      {<<"dir">>, case Record#style.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#style.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#style.dropzone},
      {<<"hidden">>, case Record#style.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#style.id},
      {<<"lang">>, Record#style.lang},
      {<<"spellcheck">>, case Record#style.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#style.style},
      {<<"tabindex">>, Record#style.tabindex},
      {<<"title">>, Record#style.title},
      {<<"translate">>, case Record#style.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"media">>, Record#style.media},
      {<<"scoped">>, case Record#style.scoped of true -> "scoped"; _ -> undefined end},      
      {<<"type">>, Record#style.type} | Record#style.data_fields
    ],
    wf_tags:emit_tag(<<"style">>, wf:render(case Record#style.body of undefined -> []; B -> B end), List).