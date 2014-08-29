-module(element_search).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, search).

render_element(Record) ->
    Id = case Record#search.postback of
        undefined -> Record#search.id;
        Postback ->
          ID = case Record#search.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#search.source, delegate=Record#search.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#search.accesskey},
      {<<"class">>, Record#search.class},
      {<<"contenteditable">>, case Record#search.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#search.contextmenu},
      {<<"dir">>, case Record#search.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#search.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#search.dropzone},
      {<<"hidden">>, case Record#search.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#search.lang},
      {<<"spellcheck">>, case Record#search.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#search.style},
      {<<"tabindex">>, Record#search.tabindex},
      {<<"title">>, Record#search.title},
      {<<"translate">>, case Record#search.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#search.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#search.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"dirname">>,Record#search.dirname},
      {<<"disabled">>, if Record#search.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#search.form},
      {<<"list">>,Record#search.list},
      {<<"maxlength">>,Record#search.maxlength},
      {<<"name">>,Record#search.name},
      {<<"pattern">>,Record#search.pattern},
      {<<"placeholder">>,Record#search.placeholder},
      {<<"readonly">>,if Record#search.readonly == true -> "readonly"; true -> undefined end},      
      {<<"required">>,if Record#search.required == true -> "required"; true -> undefined end},      
      {<<"size">>,Record#search.size},
      {<<"type">>, <<"search">>},
      {<<"value">>, wf:js_escape(Record#search.value)} | Record#search.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#search.body), List).