-module(element_upload).
-include_lib("wf.hrl").
-compile(export_all).

render_element(#upload{} = Record) ->
  Id = Record#upload.id,
  BeginPostback = wf_event:generate_postback_script(deliver_slice, ignore, Id, Record#upload.delegate, control_event, <<"{'msg': msg}">>),
  DeliverSlicePostback = wf_event:generate_postback_script(deliver_slice, ignore, Id, Record#upload.delegate, control_event, <<"{'msg': msg}">>),
  QueryFilePostback = wf_event:generate_postback_script(query_file, ignore, Id, Record#upload.delegate, control_event, <<"{'msg': msg}">>),
  CompletePostback = wf_event:generate_postback_script(complete, ignore, Id, Record#upload.delegate, control_event, <<"{'msg': msg}">>),

  wf:wire( wf:f("$(function(){ $('#~s').upload({"
    "beginUpload: function(msg){~s},"
    "deliverSlice: function(msg){~s},"
    "queryFile: function(msg){~s},"
    "complete: function(msg){~s} }); });", [Id, BeginPostback, DeliverSlicePostback, QueryFilePostback, CompletePostback]) ),

  wf_tags:emit_tag(<<"input">>, [], [
    {<<"id">>, Id},
    {<<"type">>, <<"file">>},
    {<<"class">>, Record#upload.class},
    {<<"style">>, Record#upload.style},
    {<<"name">>, Record#upload.name}
  ]).
