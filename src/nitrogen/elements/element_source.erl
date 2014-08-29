-module(element_source).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, source).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#source.accesskey},
      {<<"class">>, Record#source.class},
      {<<"contenteditable">>, case Record#source.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#source.contextmenu},
      {<<"dir">>, case Record#source.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#source.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#source.dropzone},
      {<<"hidden">>, case Record#source.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#source.id},
      {<<"lang">>, Record#source.lang},
      {<<"spellcheck">>, case Record#source.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#source.style},
      {<<"tabindex">>, Record#source.tabindex},
      {<<"title">>, Record#source.title},
      {<<"translate">>, case Record#source.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"media">>,Record#source.media},
      {<<"type">>,Record#source.type},
      {<<"src">>,Record#source.src} | Record#source.data_fields
    ],
    wf_tags:emit_tag(<<"source">>, List).