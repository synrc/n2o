-module(element_upload).
-behaviour(gen_server).
-include_lib("wf.hrl").
-include_lib("kernel/include/file.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).
-record(state, {ctl_id, name, data= <<>>, root, delegate}).

init([Init]) ->
  case file:open(Init#state.root++"/"++Init#state.name, [raw, binary, write]) of
    {ok, D} -> file:close(D), {ok, Init};
    {error, R} -> {stop, R}
  end.
handle_call({deliver_slice, Data}, _F, S) ->
  OldData = S#state.data,
  NewData = <<OldData/binary, Data/binary>>,
  {reply, {upload, erlang:byte_size(NewData)}, S#state{data=NewData}}.
handle_cast({complete, ActionHolder}, S) ->
  File = S#state.root++"/"++S#state.name,
  Data = S#state.data,
  case S#state.delegate of
    undefined -> file:write_file(File, Data, [append, raw]);
    Module -> Module:control_event(S#state.ctl_id, {File, Data, ActionHolder})
  end,
  {stop, normal, #state{}}.
handle_info(_Info, S) -> {noreply, S}.
terminate(_R, _) -> ok.
code_change(_, S, _) -> {ok, S}.

render_element(#upload{} = R) -> wire(R), render(R).

render(#upload{id=Id} = R) ->
  wf_tags:emit_tag(<<"input">>, [], [
    {<<"id">>, Id},
    {<<"type">>, <<"file">>},
    {<<"class">>, R#upload.class},
    {<<"style">>, <<"display:none">>},
    {<<"name">>, R#upload.name}
  ]).

wire(#upload{id=Id} = R) ->
  wf:wire( wf:f("$(function(){ $('#~s').upload({"
    "beginUpload: function(msg){~s},"
    "deliverSlice: function(msg){~s},"
    "queryFile: function(msg){~s},"
    "complete: function(msg){~s} }); });", [Id]++ [wf_event:generate_postback_script(Tag, ignore, Id, element_upload, control_event, <<"{'msg': msg}">>)
      || Tag <- [{begin_upload, R#upload.root, R#upload.delegate}, deliver_slice, {query_file, R#upload.root}, complete]])).


control_event(Cid, Tag) ->
  Msg = wf:q(msg),
  {Event, Param} = case Msg of
    {'query', Name} ->
      {query_file, Root} = Tag,
      {exist, case file:read_file_info(Root++"/"++binary_to_list(Name)) of {ok, FileInfo} -> FileInfo#file_info.size; {error, _} -> 0 end};
    {begin_upload, Name} ->
      {begin_upload, Root, Delegate} = Tag,
      case gen_server:start(?MODULE, [#state{ctl_id=Cid, root=Root, name=binary_to_list(Name), delegate=Delegate}], []) of
        {ok, Pid} -> {begin_upload, pid_to_list(Pid)}; {error, R} when is_atom(R) -> {error, atom_to_list(R)}; {error, R}-> {error, R} end;
    {upload, Pid, Data} -> gen_server:call(list_to_pid(binary_to_list(Pid)), {deliver_slice, Data});
    {complete, Pid} -> ActionHolder = upload, wf:reg(ActionHolder), gen_server:cast(list_to_pid(binary_to_list(Pid)), {complete, ActionHolder}), {complete, "skip"};
    M -> {error, M}
  end,
  wf:wire(wf:f("$('#~s').trigger('~p', [~p]);", [Cid, Event, Param])).
