-module(element_password).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, password).

render_element(Record) ->
    Id = case Record#password.postback of
        undefined -> Record#password.id;
        Postback ->
          ID = case Record#password.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#password.source, delegate=Record#password.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#password.accesskey},
      {<<"class">>, Record#password.class},
      {<<"contenteditable">>, case Record#password.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#password.contextmenu},
      {<<"dir">>, case Record#password.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#password.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#password.dropzone},
      {<<"hidden">>, case Record#password.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#password.lang},
      {<<"spellcheck">>, case Record#password.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#password.style},
      {<<"tabindex">>, Record#password.tabindex},
      {<<"title">>, Record#password.title},
      {<<"translate">>, case Record#password.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec 
      {<<"autocomplete">>, case Record#password.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},      
      {<<"autofocus">>,if Record#password.autofocus == true -> "autofocus"; true -> undefined end},            
      {<<"disabled">>, if Record#password.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#password.form},
      {<<"maxlength">>,Record#password.maxlength},
      {<<"name">>,Record#password.name},
      {<<"pattern">>,Record#password.pattern},
      {<<"placeholder">>, Record#password.placeholder},
      {<<"readonly">>,if Record#password.readonly == true -> "readonly"; true -> undefined end},      
      {<<"required">>,if Record#password.required == true -> "required"; true -> undefined end},      
      {<<"size">>,Record#password.size},
      {<<"type">>, <<"password">>},
      {<<"value">>,wf:js_escape(Record#password.value)} | Record#password.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#password.body), List).
