-module(action_event).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#event{ 
    postback=Postback, actions=Actions, source=Source,
    trigger=Trigger, target=Target, validation_group=ValidationGroup,
    type=Type, keycode=KeyCode, shift_key=ShiftKey, delay=Delay, delegate=Delegate,
    extra_param=ExtraParam}) ->
    Data = "[" ++ string:join([begin 
      {Key, Id} = if is_atom(Src)-> S = atom_to_list(Src), {"Bert.atom('"++S++"')", S}; true -> {"utf8.toByteArray('"++Src++"')", Src} end,
      "Bert.tuple("++Key++", utf8.toByteArray($('#"++Id++"').vals()))" end || Src <- Source ],",") ++ "]",

    Control = wf:coalesce([ValidationGroup, Trigger]),
    PostbackScript = wf_event:new(Postback, Control, Delegate, event, Data),

    [
        wf:f("$('#~s').bind('~s',function anonymous(event) { ", [Control,Type]), PostbackScript, "});"
    ].
