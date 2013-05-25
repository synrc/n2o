-module(element_h5).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, h5).

render_element(Record) ->
  wf_tags:emit_tag(<<"h5">>, wf:render(Record#h5.body), [
    {<<"id">>, Record#h5.id},
    {<<"class">>, Record#h5.class},
    {<<"style">>, Record#h5.style}
  ]).
