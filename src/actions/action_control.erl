-module(action_control).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#control{actions=Actions, source=Source, target=Target, type=Type, delegate=Delegate}) ->
    Data = "[" ++ string:join([ "Bert.tuple(Bert.atom('"++atom_to_list(Src)++
                     "'), utf8.toByteArray($('#"++atom_to_list(Src)++"').val()))" || Src <- Source ],",") ++ "]",
    PostbackScript = wf_event:new(ok, Target, Delegate, control_event, Data),
    [wf:f("$('#~s').bind('~s',function anonymous(event) { ", [Target,Type]),PostbackScript,"});"].
