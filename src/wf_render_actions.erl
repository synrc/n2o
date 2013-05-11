-module(wf_render_actions).
-author('Rusty Klophaus').
-author('Andrew Zadorozhny').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_actions(Actions) -> render_actions(Actions, undefined).
render_actions(Actions, Anchor) -> render_actions(Actions, Anchor, Anchor).
render_actions(Actions, Trigger, Target) ->
    Script = inner_render_actions(Actions, Trigger, Target),
    {ok, Script}.

inner_render_actions(Action, Trigger, Target) ->
    if
        Action == [] -> [];
        Action == undefined -> [];
        is_binary(Action)   -> [Action];
        ?IS_STRING(Action)  -> [Action];
        is_tuple(Action)    -> inner_render_action(Action, Trigger, Target);
        is_list(Action)     -> [inner_render_actions(hd(Action), Trigger, Target) |
                                inner_render_actions(tl(Action), Trigger, Target) ];
                       true -> throw({unanticipated_case_in_render_actions, Action}) end.

inner_render_action(Action, Trigger, Target) when is_tuple(Action) ->
    Base = wf_utils:get_actionbase(Action),
    Module = Base#actionbase.module, 
    case Base#actionbase.is_action == is_action of
        true -> ok;
        false -> throw({not_an_action, Action})
    end,
    case Base#actionbase.show_if of 
        true -> 
            Trigger1 = wf:coalesce([Base#actionbase.trigger, Trigger]),
            Target1  = wf:coalesce([Base#actionbase.target, Target]),
            Base1 = Base#actionbase {
                trigger = Trigger1,
                target = Target1
            },
            Action1 = wf_utils:replace_with_base(Base1, Action),
            ActionScript = call_action_render(Module, Action1, Trigger1, Target1),
            case ActionScript /= undefined andalso lists:flatten(ActionScript) /= [] of
                true  -> [ActionScript];
                false -> []
            end;
        _ -> 
            []
    end.

call_action_render(Module, Action, Trigger, Target) ->
%    {module, Module} = code:ensure_loaded(Module),
    NewActions = Module:render_action(Action),
    inner_render_actions(NewActions, Trigger, Target).

to_js_id(P) ->
    P1 = lists:reverse(P),
    string:join(P1, ".").
