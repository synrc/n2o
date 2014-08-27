-module(element_file).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, file).

render_element(Record) ->
    Id = case Record#file.postback of
        undefined -> Record#file.id;
        Postback ->
          ID = case Record#file.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#file.source, delegate=Record#file.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#file.accesskey},
      {<<"class">>, Record#file.class},
      {<<"contenteditable">>, case Record#file.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#file.contextmenu},
      {<<"dir">>, case Record#file.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#file.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#file.dropzone},
      {<<"hidden">>, case Record#file.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#file.lang},
      {<<"spellcheck">>, case Record#file.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#file.style},
      {<<"tabindex">>, Record#file.tabindex},
      {<<"title">>, Record#file.title},
      {<<"translate">>, case Record#file.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"accept">>,Record#file.accept},
      {<<"autofocus">>,if Record#file.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#file.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#file.form},
      {<<"multiple">>,if Record#file.multiple == true -> "multiple"; true -> undefined end},
      {<<"name">>,Record#file.name},
      {<<"required">>,if Record#file.required == true -> "required"; true -> undefined end}, 
      {<<"type">>, <<"file">>} | Record#file.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#file.body), List).