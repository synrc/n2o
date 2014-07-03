-module(element_param).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, param).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#param.accesskey},
      {<<"class">>, Record#param.class},
      {<<"contenteditable">>, case Record#param.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#param.contextmenu},
      {<<"dir">>, case Record#param.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#param.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#param.dropzone},
      {<<"hidden">>, case Record#param.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#param.id},
      {<<"lang">>, Record#param.lang},
      {<<"spellcheck">>, case Record#param.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#param.style},
      {<<"tabindex">>, Record#param.tabindex},
      {<<"title">>, Record#param.title},
      {<<"translate">>, case Record#param.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"name">>,Record#param.name},
      {<<"value">>,Record#param.value} | Record#param.data_fields
    ],
    wf_tags:emit_tag(<<"param">>, List).