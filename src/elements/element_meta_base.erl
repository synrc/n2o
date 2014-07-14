-module(element_meta_base).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, base).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#base.accesskey},
      {<<"class">>, Record#base.class},
      {<<"contenteditable">>, case Record#base.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#base.contextmenu},
      {<<"dir">>, case Record#base.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#base.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#base.dropzone},
      {<<"hidden">>, case Record#base.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#base.id},
      {<<"lang">>, Record#base.lang},
      {<<"spellcheck">>, case Record#base.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#base.style},
      {<<"tabindex">>, Record#base.tabindex},
      {<<"title">>, Record#base.title},
      {<<"translate">>, case Record#base.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"href">>,Record#base.href},
      {<<"target">>,Record#base.target} | Record#base.data_fields
    ],
    wf_tags:emit_tag(<<"base">>, List).