-module(element_select).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_element(Record = #select{}) ->
  ID = case Record#select.id of undefined -> wf:temp_id(); I->I end,
  case Record#select.postback of
    undefined -> skip;
    Postback -> wf:wire(#event{ type=change,
                                target=ID,
                                postback=Postback,
                                source=[wf:to_atom(ID)],
                                delegate=Record#select.delegate }) end,
  Props = [
    {<<"id">>, Record#select.id},
    {<<"class">>, Record#select.class},
    {<<"style">>, Record#select.style},
    {<<"name">>, Record#select.name},
    {<<"title">>, Record#select.title},
    {<<"disabled">>, case Record#select.disabled of true -> <<"disabled">>; _-> undefined end},
    {<<"multiple">>, case Record#select.multiple of true -> <<"multiple">>; _-> undefined end} | Record#select.data_fields
  ],
  wf_tags:emit_tag(<<"select">>, wf:render(Record#select.body),
                                  Props);
render_element(Group = #optgroup{}) ->
  wf_tags:emit_tag(<<"optgroup">>, wf:render(Group#optgroup.body), [
    {<<"disabled">>, case Group#optgroup.disabled of true-> <<"disabled">>; _-> undefined end},
    {<<"label">>, Group#optgroup.label}
  ]);
render_element(O = #option{}) ->
  wf_tags:emit_tag(<<"option">>, wf:render(O#option.body), [
    {<<"id">>, O#option.id},
    {<<"disabled">>, O#option.disabled},
    {<<"label">>, O#option.label},
    {<<"title">>, O#option.title},
    {<<"selected">>, case O#option.selected of true -> <<"selected">>; _-> undefined end},
    {<<"value">>, O#option.value} | O#option.data_fields]).
