-module (element_htmlbox).
-author('doxtop@synrc.com').
-include_lib("wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

render_element(R=#htmlbox{state=S})->
  Id = case R#htmlbox.id of undefined-> wf:temp_id(); I -> I end,
  Html = case R#htmlbox.html of undefined -> ""; H -> wf:js_escape(H) end,

  wf:wire(wf:f("Htmlbox('#~s');", [Id])),
  wf:wire(#event{postback={Id,wire_upload,S}, target=Id, type=wire_upload, delegate=R#htmlbox.delegate}),

  wf_tags:emit_tag(<<"div">>, wf:render(Html),[
    {<<"id">>, Id},
    {<<"style">>, R#htmlbox.style},
    {<<"class">>, R#htmlbox.class}]).

event({Cid, wire_upload, #upload_state{}=S})->
  [{cmd, [{name, Name}, {tag, "wire_upload"}]} = Cmd] = wf:q({Cid, <<"detail">>}),
  Uid = wf:temp_id(),
  U = #upload{id = Uid, state=S#upload_state{cid = Uid}},
  ?WS_SEND(Cid, exec, {cmd, [{name, wf:to_binary(Name)}, {arg, wf:to_binary(element_upload:render(U))}]}),
  element_upload:wire(U),
  wf:wire(#event{postback={Cid, complete, Uid}, target=Uid, type=upload_complete, delegate=?MODULE});
event({Cid, complete, Uid}) ->
  [{file, File}] = wf:q({Uid, <<"detail">>}),
  ?WS_SEND(Uid, complete_replace, {file, wf:to_binary(File)});
event(E)-> ok.
