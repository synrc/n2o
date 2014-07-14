-module(element_summary).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, summary).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#summary.accesskey},
      {<<"class">>, Record#summary.class},
      {<<"contenteditable">>, case Record#summary.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#summary.contextmenu},
      {<<"dir">>, case Record#summary.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#summary.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#summary.dropzone},
      {<<"hidden">>, case Record#summary.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#summary.id},
      {<<"lang">>, Record#summary.lang},
      {<<"spellcheck">>, case Record#summary.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#summary.style},
      {<<"tabindex">>, Record#summary.tabindex},
      {<<"title">>, Record#summary.title},
      {<<"translate">>, case Record#summary.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end} | Record#summary.data_fields
    ],
    wf_tags:emit_tag(<<"summary">>, wf:render(case Record#summary.body of undefined -> []; B -> B end), List).