-module(element_password).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, password).

render_element(Record) -> 
    List = [
      {<<"id">>, Record#password.id},
      {<<"type">>, <<"password">>},
      {<<"maxlength">>,Record#password.maxlength},
      {<<"style">>,Record#password.style},
      {<<"name">>,Record#password.name},
      {<<"placeholder">>,Record#password.placeholder},
      {<<"value">>,Record#password.value},
      {<<"class">>,Record#password.class} | Record#password.data_fields
  ],
  wf_tags:emit_tag(<<"input">>, wf:render(Record#password.body), List).

