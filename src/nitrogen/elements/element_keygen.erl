-module(element_keygen).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, keygen).

render_element(Record) ->
    Id = case Record#keygen.postback of
        undefined -> Record#keygen.id;
        Postback ->
          ID = case Record#keygen.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#keygen.source, delegate=Record#keygen.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#keygen.accesskey},
      {<<"class">>, Record#keygen.class},
      {<<"contenteditable">>, case Record#keygen.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#keygen.contextmenu},
      {<<"dir">>, case Record#keygen.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#keygen.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#keygen.dropzone},
      {<<"hidden">>, case Record#keygen.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#keygen.lang},
      {<<"spellcheck">>, case Record#keygen.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#keygen.style},
      {<<"tabindex">>, Record#keygen.tabindex},
      {<<"title">>, Record#keygen.title},
      {<<"translate">>, case Record#keygen.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autofocus">>,if Record#keygen.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"challenge">>,Record#keygen.challenge},      
      {<<"disabled">>, if Record#keygen.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#keygen.form},
      {<<"keytype">>,<<"rsa">>},
      {<<"name">>,Record#keygen.name} | Record#keygen.data_fields
    ],
    wf_tags:emit_tag(<<"keygen">>, wf:render(Record#keygen.body), List).