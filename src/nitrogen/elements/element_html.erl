-module(element_html).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, html).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#html.accesskey},
      {<<"class">>, Record#html.class},
      {<<"contenteditable">>, case Record#html.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#html.contextmenu},
      {<<"dir">>, case Record#html.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#html.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#html.dropzone},
      {<<"hidden">>, case Record#html.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#html.id},
      {<<"lang">>, Record#html.lang},
      {<<"spellcheck">>, case Record#html.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#html.style},
      {<<"tabindex">>, Record#html.tabindex},
      {<<"title">>, Record#html.title},
      {<<"translate">>, case Record#html.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"manifest">>, Record#html.manifest} | Record#html.data_fields
    ],
    wf_tags:emit_tag(<<"html">>, wf:render(case Record#html.body of undefined -> []; B -> B end), List).