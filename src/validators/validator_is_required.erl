% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_is_required).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    TriggerPath = Record#is_required.trigger,
    TargetPath = Record#is_required.target,
    Text = wf:js_escape(Record#is_required.text),
    CustomValidatorAction = #custom { trigger=TriggerPath, target=TargetPath, function=fun validate/2, text=Text, tag=Record },
    Script = wf:f("v.add(Validate.Presence, { failureMessage: \"~s\" });", [Text]),
    [CustomValidatorAction, Script].

validate(_, undefined) ->
    false;
validate(_, Value) ->
    Value /= [].
