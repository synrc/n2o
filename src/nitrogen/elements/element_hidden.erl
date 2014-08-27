-module(element_hidden).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, hidden).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#hidden.accesskey},
      {<<"class">>, Record#hidden.class},
      {<<"contenteditable">>, case Record#hidden.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#hidden.contextmenu},
      {<<"dir">>, case Record#hidden.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#hidden.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#hidden.dropzone},
      {<<"hidden">>, case Record#hidden.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#hidden.id},
      {<<"lang">>, Record#hidden.lang},
      {<<"spellcheck">>, case Record#hidden.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#hidden.style},
      {<<"tabindex">>, Record#hidden.tabindex},
      {<<"title">>, Record#hidden.title},
      {<<"translate">>, case Record#hidden.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"disabled">>, if Record#hidden.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#hidden.form},
      {<<"name">>,Record#hidden.name},
      {<<"type">>, <<"hidden">>},
      {<<"value">>, wf:js_escape(Record#hidden.value)} | Record#hidden.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#hidden.body), List).