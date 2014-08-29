-module(element_area).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, area).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#area.accesskey},
      {<<"class">>, Record#area.class},
      {<<"contenteditable">>, case Record#area.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#area.contextmenu},
      {<<"dir">>, case Record#area.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#area.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#area.dropzone},
      {<<"hidden">>, case Record#area.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#area.id},
      {<<"lang">>, Record#area.lang},
      {<<"spellcheck">>, case Record#area.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#area.style},
      {<<"tabindex">>, Record#area.tabindex},
      {<<"title">>, Record#area.title},
      {<<"translate">>, case Record#area.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"alt">>,Record#area.alt},
      {<<"coords">>,Record#area.coords},
      {<<"href">>,Record#area.href},
      {<<"hreflang">>,Record#area.hreflang},
      {<<"media">>,Record#area.media},
      {<<"rel">>,Record#area.rel},
      {<<"shape">>, case Record#area.shape of "rect" -> "rect"; "circle" -> "circle"; "poly" -> "poly"; "default" -> "default"; _ -> undefined end},
      {<<"target">>,Record#area.target},
      {<<"type">>,Record#area.type} | Record#area.data_fields
    ],
    wf_tags:emit_tag(<<"area">>, List).