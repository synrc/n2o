-module (element_blockquote).
-author('Andrew Zadorozhny').
-include("wf.hrl").
-compile(export_all).

render_element(Record) ->
  wf_tags:emit_tag(<<"blockquote">>, wf:render(Record#blockquote.body), [
      {<<"id">>, Record#blockquote.id},
      {<<"class">>, Record#blockquote.class},
      {<<"style">>, Record#blockquote.style},
      {<<"cite">>, Record#blockquote.cite}  | Record#blockquote.data_fields
  ]).
