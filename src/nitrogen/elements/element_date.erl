-module(element_date).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, date).

render_element(Record) ->
    Id = case Record#date.postback of
        undefined -> Record#date.id;
        Postback ->
          ID = case Record#date.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#date.source, delegate=Record#date.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#date.accesskey},
      {<<"class">>, Record#date.class},
      {<<"contenteditable">>, case Record#date.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#date.contextmenu},
      {<<"dir">>, case Record#date.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#date.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#date.dropzone},
      {<<"hidden">>, case Record#date.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#date.lang},
      {<<"spellcheck">>, case Record#date.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#date.style},
      {<<"tabindex">>, Record#date.tabindex},
      {<<"title">>, Record#date.title},
      {<<"translate">>, case Record#date.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#date.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#date.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#date.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#date.form},
      {<<"list">>,Record#date.list},
      {<<"max">>,Record#date.max},
      {<<"min">>,Record#date.min},
      {<<"name">>,Record#date.name},
      {<<"readonly">>,if Record#date.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#date.required == true -> "required"; true -> undefined end},      
      {<<"step">>,Record#date.step},
      {<<"type">>, <<"date">>},
      {<<"value">>,wf:js_escape(Record#date.value)} | Record#date.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#date.body), List).