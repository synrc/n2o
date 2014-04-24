-module(element_upload).
-behaviour(gen_server).
-include_lib("wf.hrl").
-include_lib("kernel/include/file.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).

-ifndef(IMG_TOOL).
-define(IMG_TOOL, (wf:config(n2o, img_tool, ?MODULE))).
-endif.
-ifndef(TH_DIR).
-define(TH_DIR, (wf:config(n2o, thumbnail_dir, "thumbnails"))).
-endif.

init([#upload_state{root=Root, dir=Dir, name=Name, index=I}=S]) ->
  case file:open(filename:join([Root, Dir, Name]), [raw, binary, write, read]) of {ok, D} ->
    ReadS =  if S#upload_state.index == 0 -> {ok, S};
      true -> case file:read(D, I) of {ok, Dl} -> {ok, S#upload_state{data=Dl}};_ -> {ok, S} end end,
    file:close(D),
    ReadS;
  {error, enoent}->{ok,S};
  {error,enotdir}->{ok,S};
  {error,R}->{stop,R} end.
handle_call({deliver_slice, Data},_, #upload_state{data=Old}=S) ->
  New = <<Old/binary, Data/binary>>,
  {reply, {delivered, erlang:byte_size(New)}, S#upload_state{data=New}}.
handle_cast(complete, #upload_state{}=S) -> {stop, normal, S}.
handle_info(_,S) -> {noreply, S}.
terminate(normal, S) ->
  File = filename:join([S#upload_state.root, S#upload_state.dir, S#upload_state.name]),
  filelib:ensure_dir(File),
  Ext = filename:extension(File),
  Name = filename:basename(File, Ext),
  ThDir = filename:join([S#upload_state.root, S#upload_state.dir,?TH_DIR]),
  {Ev,D} = case file:write_file(File, S#upload_state.data, [write, raw]) of ok ->
    [begin
      ThName = filename:join([ThDir,Name++"_"++integer_to_list(W)++"x"++integer_to_list(H)++Ext]),
      filelib:ensure_dir(ThName),
      ?IMG_TOOL:make_thumb(File,W,H,ThName)
     end || {W,H}<-S#upload_state.size],
    {upload_complete, {file, list_to_binary(filename:join(S#upload_state.dir, S#upload_state.name))}};
    {error, R} -> {error, {msg, R}} end,
  ?WS_SEND(S#upload_state.cid, Ev, D),
  wf:flush(S#upload_state.room);
terminate(R,S) ->
  case R of shutdown -> ok; {shutdown, _T} -> ok;
    E -> ?WS_SEND(S#upload_state.cid, error, [{msg, E}]), wf:flush(S#upload_state.room) end.
code_change(_,S,_) -> {ok,S}.

render_element(#upload{id=Id, state=S} = R) ->
  Uid = case Id of undefined -> wf:temp_id(); I -> I end,
  Ust = S#upload_state{cid=Uid},
  Uel = R#upload{id=Uid, state=Ust},
  wire(Uel), render(Uel).

render(#upload{id=Id} = R) ->
  wf_tags:emit_tag(<<"input">>, [], [
    {<<"id">>, Id},
    {<<"type">>, <<"file">>},
    {<<"class">>, R#upload.class},
    {<<"style">>, <<"display:none">>},
    {<<"name">>, R#upload.name} ]).

wire(#upload{id=Id, state=S, delegate=D}=R) ->
  Callbacks = [{query_file, [Id], S},{start_upload, [], S}, {deliver, [], S}, {complete, [], ok}],
  wf:wire(wf:f("Upload('#~s',{preview:'~s',value:'~s', block_size:'~s'});",
    [Id, wf:to_list(S#upload_state.preview), R#upload.value, wf:to_list(S#upload_state.block_size)])),

  [wf:wire(#event{postback={Id,T,Arg}, source=Src, target=Id, type=T, delegate=D}) || {T, Src, Arg} <- Callbacks].

event({Id, query_file, #upload_state{}=S}) ->
  [{file_name,Name}] = wf:q({Id, <<"detail">>}),
  Absname = filename:join([S#upload_state.root, S#upload_state.dir, wf:to_list(Name)]),
  Size = case file:read_file_info(Absname) of {ok, Fi} -> Fi#file_info.size; {error, _} -> 0 end,
  ?WS_SEND(Id, queried,{file_size, Size});

event({Id, start_upload, #upload_state{}=S}) ->
  [{file_name, Name}, {type, MimeType}, {index, Index}] = wf:q({Id, <<"detail">>}),
  UpS=S#upload_state{cid=Id, name=wf:to_list(Name), type=MimeType, index=Index},
  {Ev, Detail} = case gen_server:start(?MODULE, [UpS], []) of
    {ok, Pid} ->
      wf:reg(S#upload_state.room),
      {read_slice,  {pid, list_to_binary(pid_to_list(Pid))}};
    {error, R} when is_atom(R) -> {error, {msg, atom_to_list(R)}};
    {error, R} -> {error, {msg, R}} end,
  ?WS_SEND(Id, Ev, Detail);

event({Id, deliver,_}) ->
  [{pid, Pid}, {data, Data}] = wf:q({Id, <<"detail">>}),
  {Ev, Size} = gen_server:call(list_to_pid(Pid), {deliver_slice, Data}),
  ?WS_SEND(Id, Ev, {file_size, Size});

event({Id, complete,_}) ->
  [{pid, Pid}] = wf:q({Id, <<"detail">>}),
  gen_server:cast(list_to_pid(binary_to_list(Pid)), complete);

event(_) -> skip.

hash(Filename) ->
  {ok, Content} = file:read_file(Filename),
  <<Hash:160/integer>> = crypto:hash(sha, Content),
  lists:flatten(io_lib:format("~40.16.0b", [Hash])).

make_thumb(_,_,_,_) -> <<"">>.
