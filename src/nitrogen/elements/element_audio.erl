-module(element_audio).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, audio).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#audio.accesskey},
      {<<"class">>, Record#audio.class},
      {<<"contenteditable">>, case Record#audio.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#audio.contextmenu},
      {<<"dir">>, case Record#audio.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#audio.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#audio.dropzone},
      {<<"hidden">>, case Record#audio.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#audio.id},
      {<<"lang">>, Record#audio.lang},
      {<<"spellcheck">>, case Record#audio.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#audio.style},
      {<<"tabindex">>, Record#audio.tabindex},
      {<<"title">>, Record#audio.title},
      {<<"translate">>, case Record#audio.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"autoplay">>, case Record#audio.autoplay of true -> "autoplay"; _ -> undefined end},      
      {<<"controls">>, case Record#audio.controls of true -> "controls"; _ -> undefined end},      
      {<<"loop">>, case Record#audio.loop of true -> "loop"; _ -> undefined end},            
      {<<"mediagroup">>, Record#audio.mediagroup},      
      {<<"muted">>, case Record#audio.muted of true -> "muted"; _ -> undefined end},
      {<<"preload">>, case Record#audio.preload of "auto" -> "auto"; "none" -> "none"; "metadata" -> "metadata"; _ -> undefined end},
      {<<"src">>, Record#audio.src},
      {<<"width">>, Record#audio.width} | Record#audio.data_fields
    ],
    wf_tags:emit_tag(<<"audio">>, wf:render(case Record#audio.body of undefined -> []; B -> B end), List).