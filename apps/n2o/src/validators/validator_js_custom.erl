% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_js_custom).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    Text = wf:js_escape(Record#js_custom.text),
    Function = Record#js_custom.function,
    Args = Record#js_custom.args,
    wf:f("v.add(Validate.Custom, { against: ~s, args: ~s, failureMessage: \"~s\" });", [Function, Args, Text]).
