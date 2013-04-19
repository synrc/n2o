% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validate).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    % Some values...
    TriggerPath = Record#validate.trigger,
    TargetPath = Record#validate.target,
    ValidationGroup = case Record#validate.group of
        undefined -> TriggerPath;
        Other -> Other
    end,
    ValidMessage = wf:js_escape(Record#validate.success_text),
    OnlyOnBlur = (Record#validate.on == blur),
    OnlyOnSubmit = (Record#validate.on == submit),	
    InsertAfterNode = case Record#validate.attach_to of
        undefined -> "";
        Node -> wf:f(", insertAfterWhatNode : obj(\"~s\")", [Node])
    end,

    % Create the validator Javascript...
	ConstructorJS = wf:f("var v = Nitrogen.$add_validation(obj('~s'), { validMessage: \"~s\", onlyOnBlur: ~s, onlyOnSubmit: ~s ~s});", [TargetPath, wf:js_escape(ValidMessage), OnlyOnBlur, OnlyOnSubmit, InsertAfterNode]),

    TriggerJS = wf:f("v.group = '~s';", [ValidationGroup]),

    % Update all child validators with TriggerPath and TargetPath...
    F = fun(X) ->
        Base = wf_utils:get_validatorbase(X),
        Base1 = Base#validatorbase { trigger = TriggerPath, target = TargetPath },
        wf_utils:replace_with_base(Base1, X)
    end,
    Validators = lists:flatten([Record#validate.validators]),
    Validators1 = [F(X) || X <- Validators],

    % Use #script element to create the final javascript to send to the browser...
    [
        ConstructorJS, TriggerJS, Validators1
    ].	
