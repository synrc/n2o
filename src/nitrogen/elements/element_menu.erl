-module(element_menu).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, menu).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#menu.accesskey},
      {<<"class">>, Record#menu.class},
      {<<"contenteditable">>, case Record#menu.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#menu.contextmenu},
      {<<"dir">>, case Record#menu.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#menu.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#menu.dropzone},
      {<<"hidden">>, case Record#menu.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#menu.id},
      {<<"lang">>, Record#menu.lang},
      {<<"spellcheck">>, case Record#menu.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#menu.style},
      {<<"tabindex">>, Record#menu.tabindex},
      {<<"title">>, Record#menu.title},
      {<<"translate">>, case Record#menu.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"label">>, Record#menu.label},
      {<<"type">>, case Record#menu.type of "toolbar" -> "toolbar"; "context" -> "context"; _ -> undefined end} | Record#menu.data_fields
    ],
    wf_tags:emit_tag(<<"menu">>, wf:render(case Record#menu.body of undefined -> []; B -> B end), List).