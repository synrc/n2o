-module(element_input_button).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, input_button).

render_element(Record) ->
    Id = case Record#input_button.postback of
        undefined -> Record#input_button.id;
        Postback ->
          ID = case Record#input_button.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#input_button.source, delegate=Record#input_button.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#input_button.accesskey},
      {<<"class">>, Record#input_button.class},
      {<<"contenteditable">>, case Record#input_button.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#input_button.contextmenu},
      {<<"dir">>, case Record#input_button.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#input_button.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#input_button.dropzone},
      {<<"hidden">>, case Record#input_button.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#input_button.lang},
      {<<"spellcheck">>, case Record#input_button.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#input_button.style},
      {<<"tabindex">>, Record#input_button.tabindex},
      {<<"title">>, Record#input_button.title},
      {<<"translate">>, case Record#input_button.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autofocus">>,Record#input_button.autofocus},
      {<<"disabled">>, if Record#input_button.disabled == true -> "disabled"; true -> undefined end},
      {<<"name">>,Record#input_button.name},
      {<<"type">>, <<"button">>},
      {<<"value">>,wf:js_escape(Record#input_button.value)} | Record#input_button.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#input_button.body), List).