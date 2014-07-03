-module(element_object).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, object).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#object.accesskey},
      {<<"class">>, Record#object.class},
      {<<"contenteditable">>, case Record#object.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#object.contextmenu},
      {<<"dir">>, case Record#object.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#object.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#object.dropzone},
      {<<"hidden">>, case Record#object.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#object.id},
      {<<"lang">>, Record#object.lang},
      {<<"spellcheck">>, case Record#object.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#object.style},
      {<<"tabindex">>, Record#object.tabindex},
      {<<"title">>, Record#object.title},
      {<<"translate">>, case Record#object.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"data">>,Record#object.data},      
      {<<"form">>,Record#object.form},      
      {<<"height">>,Record#object.height},      
      {<<"name">>,Record#object.name},            
      {<<"type">>,Record#object.type},
      {<<"usemap">>,Record#object.usemap},            
      {<<"width">>,Record#object.width} | Record#object.data_fields
    ],
    wf_tags:emit_tag(<<"object">>, wf:render(case Record#object.body of undefined -> []; B -> B end), List).