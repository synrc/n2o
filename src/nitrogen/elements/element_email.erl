-module(element_email).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, email).

render_element(Record) ->
    Id = case Record#email.postback of
        undefined -> Record#email.id;
        Postback ->
          ID = case Record#email.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#email.source, delegate=Record#email.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#email.accesskey},
      {<<"class">>, Record#email.class},
      {<<"contenteditable">>, case Record#email.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#email.contextmenu},
      {<<"dir">>, case Record#email.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#email.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#email.dropzone},
      {<<"hidden">>, case Record#email.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#email.lang},
      {<<"spellcheck">>, case Record#email.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#email.style},
      {<<"tabindex">>, Record#email.tabindex},
      {<<"title">>, Record#email.title},
      {<<"translate">>, case Record#email.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#email.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#email.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#email.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#email.form},
      {<<"list">>,Record#email.list},
      {<<"maxlength">>,Record#email.maxlength},
      {<<"multiple">>,if Record#email.multiple == true -> "multiple"; true -> undefined end},
      {<<"name">>,Record#email.name},
      {<<"pattern">>,Record#email.pattern},
      {<<"placeholder">>,Record#email.placeholder},
      {<<"readonly">>,if Record#email.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#email.required == true -> "required"; true -> undefined end}, 
      {<<"size">>,Record#email.size},
      {<<"type">>, <<"email">>},
      {<<"value">>,wf:js_escape(Record#email.value)} | Record#email.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#email.body), List).