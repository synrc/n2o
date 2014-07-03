-module(element_video).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, video).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#video.accesskey},
      {<<"class">>, Record#video.class},
      {<<"contenteditable">>, case Record#video.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#video.contextmenu},
      {<<"dir">>, case Record#video.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#video.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#video.dropzone},
      {<<"hidden">>, case Record#video.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#video.id},
      {<<"lang">>, Record#video.lang},
      {<<"spellcheck">>, case Record#video.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#video.style},
      {<<"tabindex">>, Record#video.tabindex},
      {<<"title">>, Record#video.title},
      {<<"translate">>, case Record#video.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autoplay">>, case Record#video.autoplay of true -> "autoplay"; _ -> undefined end},      
      {<<"controls">>, case Record#video.controls of true -> "controls"; _ -> undefined end},      
      {<<"height">>, Record#video.height},      
      {<<"loop">>, case Record#video.loop of true -> "loop"; _ -> undefined end},            
      {<<"mediagroup">>, Record#video.mediagroup},      
      {<<"muted">>, case Record#video.muted of true -> "muted"; _ -> undefined end},
      {<<"poster">>, Record#video.poster},      
      {<<"preload">>, case Record#video.preload of "auto" -> "auto"; "none" -> "none"; "metadata" -> "metadata"; _ -> undefined end},
      {<<"src">>, Record#video.src},     
      {<<"width">>, Record#video.width} | Record#video.data_fields
    ],
    wf_tags:emit_tag(<<"video">>, wf:render(case Record#video.body of undefined -> []; B -> B end), List).