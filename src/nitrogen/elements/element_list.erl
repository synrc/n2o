-module(element_list).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record = #list{}) -> 
  Tag = case Record#list.numbered of true -> <<"ol">>; _ -> <<"ul">> end,

  wf_tags:emit_tag(Tag, wf:render(Record#list.body), [
    {<<"id">>, Record#list.id},
    {<<"class">>, Record#list.class},
    {<<"style">>, Record#list.style} | Record#list.data_fields
  ]).

