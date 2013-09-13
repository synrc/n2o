-module(element_tbody).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record = #tbody{}) ->
  wf_tags:emit_tag( <<"tbody">>, wf:render(Record#tbody.body), [
      {<<"id">>, Record#tbody.id},
      {<<"class">>, Record#tbody.class},
      {<<"style">>, Record#tbody.style}
  ]).