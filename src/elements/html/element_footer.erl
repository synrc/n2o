-module(element_footer).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, footer).

render_element(Record = #footer{}) ->
  wf_tags:emit_tag(<<"footer">>, wf:render(Record#footer.body), [
    {<<"id">>, Record#footer.id},
    {<<"class">>, Record#footer.class},
    {<<"style">>, Record#footer.style}
  ]).
