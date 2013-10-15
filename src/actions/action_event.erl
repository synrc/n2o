-module(action_event).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(#event{source=undefined}) -> [];
render_action(#event{postback=Postback,actions=Actions,source=Source,target=Control,type=Type,delegate=Delegate}) ->
    Data = "[" ++ string:join([begin 
        {Key, Id} = if  is_atom(Src)-> S = atom_to_list(Src),
                        {"Bert.atom('"++S++"')", S};
                    true -> {"utf8.toByteArray('" ++ Src ++ "')", Src} end,
        "Bert.tuple(" ++ Key ++ ", utf8.toByteArray($('#" ++ Id ++ "').vals()))" end
    || Src <- Source ],",") ++ "]",
    PostbackScript = wf_event:new(Postback, Control, Delegate, event, Data),
    [ wf:f("$('#~s').bind('~s',function anonymous(event) { ", [Control,Type]),PostbackScript,"});"].
