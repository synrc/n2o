-module(element_range).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, range).

render_element(Record) ->
    Id = case Record#range.postback of
        undefined -> Record#range.id;
        Postback ->
          ID = case Record#range.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#range.source, delegate=Record#range.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#range.accesskey},
      {<<"class">>, Record#range.class},
      {<<"contenteditable">>, case Record#range.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#range.contextmenu},
      {<<"dir">>, case Record#range.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#range.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#range.dropzone},
      {<<"hidden">>, case Record#range.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#range.lang},
      {<<"spellcheck">>, case Record#range.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#range.style},
      {<<"tabindex">>, Record#range.tabindex},
      {<<"title">>, Record#range.title},
      {<<"translate">>, case Record#range.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#range.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#range.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#range.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#range.form},
      {<<"list">>,Record#range.list},
      {<<"max">>,Record#range.max},
      {<<"min">>,Record#range.min},
      {<<"name">>,Record#range.name},
      {<<"step">>,Record#range.step},
      {<<"type">>, <<"range">>},
      {<<"value">>, wf:js_escape(Record#range.value)} | Record#range.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#range.body), List).