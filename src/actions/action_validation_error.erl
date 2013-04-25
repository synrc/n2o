% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_validation_error).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    TargetPath = Record#validation_error.target,
    Text = wf:js_escape(Record#validation_error.text),
    [
		%% Is this going to be a problem with memory leaks? I'm not sure.
        wf:f("var v = new LiveValidation(obj('~s'), { onlyOnSubmit: true });", [TargetPath]),
        wf:f("v.add(Validate.Custom, { against: Nitrogen.$return_false, failureMessage: \"~s\", displayMessageWhenEmpty: true });", [Text]),
        "v.validate();"
    ].


