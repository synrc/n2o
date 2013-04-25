-module(action_event).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#event{ 
    postback=Postback, actions=Actions, 
    anchor=Anchor, trigger=Trigger, target=Target, validation_group=ValidationGroup,
    type=Type, keycode=KeyCode, shift_key=ShiftKey, delay=Delay, delegate=Delegate,
    extra_param=ExtraParam}) ->

    ValidationGroup1 = wf:coalesce([ValidationGroup, Trigger]),
    PostbackScript = wf_event:generate_postback_script(Postback, Anchor, ValidationGroup1, Delegate, ExtraParam),
    SystemPostbackScript = wf_event:generate_system_postback_script(Postback, Anchor, ValidationGroup1, Delegate),
    WireAction = #wire { trigger=Trigger, target=Target, actions=Actions },

    [
        wf:f("$('#~s').bind('click',function anonymous(event) { ", [ValidationGroup]), PostbackScript, WireAction, "});"
    ].
