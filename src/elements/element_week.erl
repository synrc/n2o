-module(element_week).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, week).

render_element(Record) ->
    Id = case Record#week.postback of
        undefined -> Record#week.id;
        Postback ->
          ID = case Record#week.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#week.source, delegate=Record#week.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#week.accesskey},
      {<<"class">>, Record#week.class},
      {<<"contenteditable">>, case Record#week.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#week.contextmenu},
      {<<"dir">>, case Record#week.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#week.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#week.dropzone},
      {<<"hidden">>, case Record#week.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#week.lang},
      {<<"spellcheck">>, case Record#week.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#week.style},
      {<<"tabindex">>, Record#week.tabindex},
      {<<"title">>, Record#week.title},
      {<<"translate">>, case Record#week.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#week.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#week.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#week.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#week.form},
      {<<"list">>,Record#week.list},
      {<<"max">>,Record#week.max},
      {<<"min">>,Record#week.min},
      {<<"name">>,Record#week.name},
      {<<"readonly">>,if Record#week.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#week.required == true -> "required"; true -> undefined end},      
      {<<"step">>,Record#week.step},
      {<<"type">>, <<"week">>},
      {<<"value">>,wf:js_escape(Record#week.value)} | Record#week.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#week.body), List).