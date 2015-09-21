-module(upload).
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").

-record(upload, {?CTRL_BASE(upload), name, value}).

render_element(#upload{id=Id} = R) ->
  Uid = case Id of undefined -> wf:temp_id(); I -> I end,
  Js = wf:json([{value,R#upload.value}]),
  wf:wire(wf:f("Upload('#~s',~s);", [Uid, Js])),
  E = wf_tags:emit_tag(<<"input">>, nitro:render(R#upload.body), [
    {<<"id">>, Id},
    {<<"type">>, <<"file">>},
    {<<"class">>, R#upload.class},
    {<<"style">>, <<"display:none">>},
    {<<"name">>, R#upload.name} ]),
  wf:info(index,"Render: ~p~n",[wf:to_binary(E)]),
  wf:to_binary(E).
