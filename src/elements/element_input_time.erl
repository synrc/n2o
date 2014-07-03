-module(element_input_time).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, input_time).

render_element(Record) ->
    Id = case Record#input_time.postback of
        undefined -> Record#input_time.id;
        Postback ->
          ID = case Record#input_time.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#input_time.source, delegate=Record#input_time.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#input_time.accesskey},
      {<<"class">>, Record#input_time.class},
      {<<"contenteditable">>, case Record#input_time.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#input_time.contextmenu},
      {<<"dir">>, case Record#input_time.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#input_time.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#input_time.dropzone},
      {<<"hidden">>, case Record#input_time.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#input_time.lang},
      {<<"spellcheck">>, case Record#input_time.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#input_time.style},
      {<<"tabindex">>, Record#input_time.tabindex},
      {<<"title">>, Record#input_time.title},
      {<<"translate">>, case Record#input_time.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#input_time.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#input_time.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#input_time.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#input_time.form},
      {<<"list">>,Record#input_time.list},
      {<<"max">>,Record#input_time.max},
      {<<"min">>,Record#input_time.min},
      {<<"name">>,Record#input_time.name},
      {<<"readonly">>,if Record#input_time.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#input_time.required == true -> "required"; true -> undefined end},      
      {<<"step">>,Record#input_time.step},
      {<<"type">>, <<"time">>},
      {<<"value">>,wf:js_escape(Record#input_time.value)} | Record#input_time.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#input_time.body), List).