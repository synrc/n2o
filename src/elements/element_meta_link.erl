-module(element_meta_link).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, meta_link).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#meta_link.accesskey},
      {<<"class">>, Record#meta_link.class},
      {<<"contenteditable">>, case Record#meta_link.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#meta_link.contextmenu},
      {<<"dir">>, case Record#meta_link.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#meta_link.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#meta_link.dropzone},
      {<<"hidden">>, case Record#meta_link.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#meta_link.id},
      {<<"lang">>, Record#meta_link.lang},
      {<<"spellcheck">>, case Record#meta_link.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#meta_link.style},
      {<<"tabindex">>, Record#meta_link.tabindex},
      {<<"title">>, Record#meta_link.title},
      {<<"translate">>, case Record#meta_link.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"href">>,Record#meta_link.href},
      {<<"hreflang">>,Record#meta_link.hreflang},
      {<<"media">>,Record#meta_link.media},
      {<<"rel">>,Record#meta_link.rel},
      {<<"sizes">>,Record#meta_link.sizes},
      {<<"type">>,Record#meta_link.type} | Record#meta_link.data_fields
    ],
    wf_tags:emit_tag(<<"link">>, List).