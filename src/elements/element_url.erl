-module(element_url).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, url).

render_element(Record) ->
    Id = case Record#url.postback of
        undefined -> Record#url.id;
        Postback ->
          ID = case Record#url.id of
            undefined -> wf:temp_id();
            I -> I end,
          wf:wire(#event{type=click, postback=Postback, target=ID,
                  source=Record#url.source, delegate=Record#url.delegate }),
          ID end,
    List = [
      %global
      {<<"accesskey">>, Record#url.accesskey},
      {<<"class">>, Record#url.class},
      {<<"contenteditable">>, case Record#url.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#url.contextmenu},
      {<<"dir">>, case Record#url.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#url.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#url.dropzone},
      {<<"hidden">>, case Record#url.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#url.lang},
      {<<"spellcheck">>, case Record#url.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#url.style},
      {<<"tabindex">>, Record#url.tabindex},
      {<<"title">>, Record#url.title},
      {<<"translate">>, case Record#url.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autocomplete">>, case Record#url.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"autofocus">>,if Record#url.autofocus == true -> "autofocus"; true -> undefined end},
      {<<"disabled">>, if Record#url.disabled == true -> "disabled"; true -> undefined end},
      {<<"form">>,Record#url.form},
      {<<"list">>,Record#url.list},
      {<<"maxlength">>,Record#url.maxlength},
      {<<"name">>,Record#url.name},
      {<<"pattern">>,Record#url.pattern},      
      {<<"placeholder">>,Record#url.placeholder},      
      {<<"readonly">>,if Record#url.readonly == true -> "readonly"; true -> undefined end},
      {<<"required">>,if Record#url.required == true -> "required"; true -> undefined end},      
      {<<"size">>,Record#url.size},
      {<<"type">>, <<"url">>},
      {<<"value">>,wf:js_escape(Record#url.value)} | Record#url.data_fields
    ],
    wf_tags:emit_tag(<<"input">>, wf:render(Record#url.body), List).