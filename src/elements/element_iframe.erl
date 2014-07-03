-module(element_iframe).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, iframe).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#iframe.accesskey},
      {<<"class">>, Record#iframe.class},
      {<<"contenteditable">>, case Record#iframe.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#iframe.contextmenu},
      {<<"dir">>, case Record#iframe.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#iframe.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#iframe.dropzone},
      {<<"hidden">>, case Record#iframe.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#iframe.id},
      {<<"lang">>, Record#iframe.lang},
      {<<"spellcheck">>, case Record#iframe.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#iframe.style},
      {<<"tabindex">>, Record#iframe.tabindex},
      {<<"title">>, Record#iframe.title},
      {<<"translate">>, case Record#iframe.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"height">>,Record#iframe.height},      
      {<<"sandbox">>,Record#iframe.sandbox},      
      {<<"seamless">>, if Record#iframe.seamless == true -> "seamless"; true -> undefined end},
      {<<"src">>,Record#iframe.src},
      {<<"srcdoc">>,Record#iframe.srcdoc},            
      {<<"name">>,Record#iframe.name},
      {<<"width">>,Record#iframe.width} | Record#iframe.data_fields
    ],
    wf_tags:emit_tag(<<"iframe">>, [], List).