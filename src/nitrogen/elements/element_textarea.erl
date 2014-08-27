-module(element_textarea).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, textarea).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#textarea.accesskey},
      {<<"class">>, Record#textarea.class},
      {<<"contenteditable">>, case Record#textarea.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#textarea.contextmenu},
      {<<"dir">>, case Record#textarea.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#textarea.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#textarea.dropzone},
      {<<"hidden">>, case Record#textarea.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#textarea.id},
      {<<"lang">>, Record#textarea.lang},
      {<<"spellcheck">>, case Record#textarea.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#textarea.style},
      {<<"tabindex">>, Record#textarea.tabindex},
      {<<"title">>, Record#textarea.title},
      {<<"translate">>, case Record#textarea.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autofocus">>,if Record#textarea.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"cols">>,Record#textarea.cols},
      {<<"dirname">>,Record#textarea.dirname},      
      {<<"disabled">>, if Record#textarea.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#textarea.form},
      {<<"maxlength">>,Record#textarea.maxlength},      
      {<<"name">>,Record#textarea.name},
      {<<"placeholder">>,Record#textarea.placeholder},
      {<<"readonly">>,if Record#textarea.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#textarea.required == true -> "required"; true -> undefined end},
      {<<"rows">>,Record#textarea.rows},      
      {<<"form">>,Record#textarea.wrap},
      {<<"value">>,Record#textarea.value},
      {<<"wrap">>, case Record#textarea.wrap of "hard" -> "hard"; "soft" -> "soft"; _ -> undefined end} | Record#textarea.data_fields
    ],
    wf_tags:emit_tag(<<"textarea">>, wf:render(case Record#textarea.body of undefined -> []; B -> B end), List).