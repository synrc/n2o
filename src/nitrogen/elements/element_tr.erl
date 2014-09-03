-module(element_tr).
-include("wf.hrl").
-compile(export_all).

render_element(Record = #tr{postback= Postback}) ->
  Id = case Record#tr.id of undefined -> wf:temp_id(); I->I end,
  Cursor = case Postback of undefined -> "";
    P -> wf:wire(#event {type=click, postback=P, target=Id, delegate=Record#tr.delegate}), "cursor:pointer;"
  end,
  wf_tags:emit_tag(<<"tr">>, wf:render(Record#tr.cells), [
    {<<"id">>, Record#tr.id},
    {<<"class">>, Record#tr.class},
    {<<"style">>, [Record#tr.style, Cursor]}
  ]).
