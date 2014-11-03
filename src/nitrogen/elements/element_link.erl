-module(element_link).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, link).

render_element(Record) -> 
    Id = case Record#link.postback of
        undefined -> Record#link.id;
        Postback ->
            ID = case Record#link.id of undefined -> wf:temp_id(); I -> I end,
            wf:wire(#event{ type=click,postback=Postback,target=ID,
                            source=Record#link.source,delegate=Record#link.delegate}),
            ID end,
    List = [
      % global
      {<<"accesskey">>, Record#link.accesskey},
      {<<"class">>, Record#link.class},
      {<<"contenteditable">>, case Record#link.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#link.contextmenu},
      {<<"dir">>, case Record#link.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#link.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#link.dropzone},
      {<<"hidden">>, case Record#link.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Id},
      {<<"lang">>, Record#link.lang},
      {<<"spellcheck">>, case Record#link.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#link.style},
      {<<"tabindex">>, Record#link.tabindex},
      {<<"title">>, Record#link.title},
      {<<"translate">>, case Record#link.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"href">>, wf:coalesce([Record#link.href,Record#link.url])},
      {<<"hreflang">>, Record#link.hreflang},
      {<<"target">>, Record#link.target},
      {<<"media">>, Record#link.media},
      {<<"rel">>, Record#link.rel},
      {<<"type">>, Record#link.type},
      {<<"download">>, Record#link.download},
      {<<"name">>, Record#link.name},
      {<<"onclick">>, Record#link.onclick},
      {<<"onmouseover">>, Record#link.onmouseover} | Record#link.data_fields ],
    wf_tags:emit_tag(<<"a">>, wf:render(Record#link.body), List).
