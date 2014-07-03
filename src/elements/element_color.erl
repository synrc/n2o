-module(element_color).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, color).

render_element(Record) ->
    Id = case Record#color.postback of
        undefined -> Record#color.id;
        Postback ->
          ID = case Record#color.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#color.source, delegate=Record#color.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#color.accesskey},
      {<<"class">>, Record#color.class},
      {<<"contenteditable">>, case Record#color.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#color.contextmenu},
      {<<"dir">>, case Record#color.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#color.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#color.dropzone},
      {<<"hidden">>, case Record#color.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#color.lang},
      {<<"spellcheck">>, case Record#color.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#color.style},
      {<<"tabindex">>, Record#color.tabindex},
      {<<"title">>, Record#color.title},
      {<<"translate">>, case Record#color.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>,case Record#color.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#color.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#color.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#color.form},
      {<<"list">>,Record#color.list},      
      {<<"name">>,Record#color.name},
      {<<"type">>, <<"color">>},
      {<<"value">>,wf:js_escape(Record#color.value)} | Record#color.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#color.body), List).