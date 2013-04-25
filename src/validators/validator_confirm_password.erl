% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_confirm_password).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record)  ->
    TriggerPath= Record#confirm_password.trigger,
    TargetPath = Record#confirm_password.target,
    Text = wf:js_escape(Record#confirm_password.text),
    PasswordElement = Record#confirm_password.password,

    validator_custom:render_action(#custom { trigger=TriggerPath, target=TargetPath, function=fun validate/2, text = Text, tag=Record }),

    JSFunction = wf:f("function(value, args) { return (value == obj('~s').value); }", [PasswordElement]),
    validator_js_custom:render_action(#js_custom { trigger=TriggerPath, target=TargetPath, function=JSFunction, text=Text }).

validate(Record, Value) ->
    Password = wf:q(Record#confirm_password.password),
    Value == Password.
