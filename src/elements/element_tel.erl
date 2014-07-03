-module(element_tel).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, tel).

render_element(Record) ->
    Id = case Record#tel.postback of
        undefined -> Record#tel.id;
        Postback ->
          ID = case Record#tel.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#tel.source, delegate=Record#tel.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#tel.accesskey},
      {<<"class">>, Record#tel.class},
      {<<"contenteditable">>, case Record#tel.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#tel.contextmenu},
      {<<"dir">>, case Record#tel.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#tel.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#tel.dropzone},
      {<<"hidden">>, case Record#tel.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#tel.lang},
      {<<"spellcheck">>, case Record#tel.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#tel.style},
      {<<"tabindex">>, Record#tel.tabindex},
      {<<"title">>, Record#tel.title},
      {<<"translate">>, case Record#tel.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#tel.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#tel.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#tel.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#tel.form},
      {<<"list">>,Record#tel.list},
      {<<"maxlength">>,Record#tel.maxlength},
      {<<"name">>,Record#tel.name},
      {<<"pattern">>,Record#tel.pattern},
      {<<"placeholder">>,Record#tel.placeholder},
      {<<"readonly">>,if Record#tel.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#tel.required == true -> "required"; true -> undefined end},      
      {<<"size">>,Record#tel.size},
      {<<"type">>, <<"tel">>},
      {<<"value">>,wf:js_escape(Record#tel.value)} | Record#tel.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#tel.body), List).