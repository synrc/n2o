-module(element_datetime).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, datetime).

render_element(Record) ->
    Id = case Record#datetime.postback of
        undefined -> Record#datetime.id;
        Postback ->
          ID = case Record#datetime.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#datetime.source, delegate=Record#datetime.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#datetime.accesskey},
      {<<"class">>, Record#datetime.class},
      {<<"contenteditable">>, case Record#datetime.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#datetime.contextmenu},
      {<<"dir">>, case Record#datetime.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#datetime.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#datetime.dropzone},
      {<<"hidden">>, case Record#datetime.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#datetime.lang},
      {<<"spellcheck">>, case Record#datetime.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#datetime.style},
      {<<"tabindex">>, Record#datetime.tabindex},
      {<<"title">>, Record#datetime.title},
      {<<"translate">>, case Record#datetime.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#datetime.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#datetime.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#datetime.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#datetime.form},
      {<<"list">>,Record#datetime.list},
      {<<"max">>,Record#datetime.max},
      {<<"min">>,Record#datetime.min},
      {<<"name">>,Record#datetime.name},
      {<<"readonly">>,if Record#datetime.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#datetime.required == true -> "required"; true -> undefined end},      
      {<<"step">>,Record#datetime.step},
      {<<"type">>, <<"datetime">>},
      {<<"value">>,wf:js_escape(Record#datetime.value)} | Record#datetime.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#datetime.body), List).