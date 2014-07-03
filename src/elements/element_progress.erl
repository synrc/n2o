-module(element_progress).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, progress).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#progress.accesskey},
      {<<"class">>, Record#progress.class},
      {<<"contenteditable">>, case Record#progress.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#progress.contextmenu},
      {<<"dir">>, case Record#progress.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#progress.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#progress.dropzone},
      {<<"hidden">>, case Record#progress.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#progress.id},
      {<<"lang">>, Record#progress.lang},
      {<<"spellcheck">>, case Record#progress.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#progress.style},
      {<<"tabindex">>, Record#progress.tabindex},
      {<<"title">>, Record#progress.title},
      {<<"translate">>, case Record#progress.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"max">>,Record#progress.max},
      {<<"value">>,Record#progress.value} | Record#progress.data_fields
    ],
    wf_tags:emit_tag(<<"progress">>, wf:render(case Record#progress.body of undefined -> []; B -> B end), List).