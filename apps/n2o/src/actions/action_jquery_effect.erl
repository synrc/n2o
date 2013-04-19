% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_jquery_effect).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    Target = Record#jquery_effect.target,
    Effect = Record#jquery_effect.effect,
    Speed = Record#jquery_effect.speed, 
    Options = options_to_js(Record#jquery_effect.options),
    Class = Record#jquery_effect.class,
    Easing = Record#jquery_effect.easing,
    Actions = case Record#jquery_effect.actions of
        undefined ->
            "null";
        Actions1 ->
            ["function() {", Actions1, "}"]
    end,

    Script = case Record#jquery_effect.type of
        'show' when Effect==none -> ["show(", Actions, ");"];
        'hide' when Effect==none -> ["hide(", Actions, ");"];
        'toggle' when Effect==none -> ["toggle(", Actions, ");"];
        'appear' -> [wf:f("fadeIn(~p, ", [Speed]), Actions, ");"];
        'fade'   -> [wf:f("fadeOut(~p, ", [Speed]), Actions, ");"];
        'slideup'-> [wf:f("slideUp(~p, ",[Speed]), Actions, ");"];
        'slidedown'    -> [wf:f("slideDown(~p, ",[Speed]), Actions, ");"];
        'show'   -> [wf:f("show('~s', ~s, ~p, ", [Effect, Options, Speed]), Actions, ");"];
        'hide'   -> [wf:f("hide('~s', ~s, ~p, ", [Effect, Options, Speed]), Actions, ");"];
        'effect' -> [wf:f("effect('~s', ~s, ~p, ", [Effect, Options, Speed]), Actions, ");"];
        'toggle' -> [wf:f("toggle('~s', ~s, ~p, ", [Effect, Options, Speed]), Actions, ");"];
        'add_class'    -> [wf:f("addClass('~s', ~p, ", [Class, Speed]), Actions, ");"];
        'remove_class' -> [wf:f("removeClass('~s', ~p, ", [Class, Speed]), Actions, ");"];
        'animate' -> [wf:f("animate(~s, ~p, '~s', ", [Options, Speed, Easing]), Actions, ");"]
    end,
    [wf:f("objs('~s').", [Target]), Script].


%% Options is a list of {Key,Value} tuples	
options_to_js(Options) ->
    F = fun({Key, Value}) ->
        if 
            is_list(Value) -> 
                wf:f("~s: '~s'", [Key, wf:js_escape(Value)]);
            is_atom(Value) andalso (Value == true orelse Value == false) ->
                wf:f("~s: ~s", [Key, Value]);
            is_atom(Value) ->
                wf:f("~s: '~s'", [Key, Value]);
            true -> 
                wf:f("~s: ~p", [Key, Value])
        end
    end,
    Options1 = [F(X) || X <- Options],
    Options2 = string:join(Options1, ","),
    wf:f("{ ~s }", [Options2]).




