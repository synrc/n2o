-module(element_form).
-author('Vladimir Galunshchikov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, form).

render_element(Record) ->
    List = [
      %global
      {<<"accesskey">>, Record#form.accesskey},
      {<<"class">>, Record#form.class},
      {<<"contenteditable">>, case Record#form.contenteditable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"contextmenu">>, Record#form.contextmenu},
      {<<"dir">>, case Record#form.dir of "ltr" -> "ltr"; "rtl" -> "rtl"; "auto" -> "auto"; _ -> undefined end},
      {<<"draggable">>, case Record#form.draggable of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"dropzone">>, Record#form.dropzone},
      {<<"hidden">>, case Record#form.hidden of "hidden" -> "hidden"; _ -> undefined end},
      {<<"id">>, Record#form.id},
      {<<"lang">>, Record#form.lang},
      {<<"spellcheck">>, case Record#form.spellcheck of true -> "true"; false -> "false"; _ -> undefined end},
      {<<"style">>, Record#form.style},
      {<<"tabindex">>, Record#form.tabindex},
      {<<"title">>, Record#form.title},
      {<<"translate">>, case Record#form.contenteditable of "yes" -> "yes"; "no" -> "no"; _ -> undefined end},      
      % spec
      {<<"accept-charset">>, Record#form.accept_charset},      
      {<<"action">>, Record#form.action},      
      {<<"autocomplete">>, case Record#form.autocomplete of true -> "on"; false -> "off"; _ -> undefined end},
      {<<"enctype">>, case Record#form.enctype of "application/x-www-form-urlencoded" -> "application/x-www-form-urlencoded"; "multipart/form-data" -> "multipart/form-data"; "text/plain" -> "text/plain"; _ -> undefined end},
      {<<"method">>, case Record#form.method of "post" -> "post"; _ -> "get" end},
      {<<"name">>,Record#form.name},
      {<<"novalidate">>, case Record#form.novalidate of true -> "novalidate"; _ -> undefined end},
      {<<"target">>, Record#form.target} | Record#form.data_fields
    ],
    wf_tags:emit_tag(<<"form">>, wf:render(Record#form.body), List).