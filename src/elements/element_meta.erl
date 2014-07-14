-module(element_meta).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, meta).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#meta.accesskey},
      {<<"class">>, Record#meta.class},
      {<<"contenteditable">>, case Record#meta.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#meta.contextmenu},
      {<<"dir">>, case Record#meta.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#meta.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#meta.dropzone},
      {<<"hidden">>, case Record#meta.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#meta.id},
      {<<"lang">>, Record#meta.lang},
      {<<"spellcheck">>, case Record#meta.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#meta.style},
      {<<"tabindex">>, Record#meta.tabindex},
      {<<"title">>, Record#meta.title},
      {<<"translate">>, case Record#meta.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"charset">>, Record#meta.charset},
      {<<"content">>, Record#meta.content},
      {<<"http_equiv">>, Record#meta.http_equiv},
      {<<"name">>, Record#meta.name},
      {<<"type">>, Record#meta.type} | Record#meta.data_fields
    ],
    wf_tags:emit_tag(<<"meta">>, List).