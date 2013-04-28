% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (validator_is_integer).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    TriggerPath = Record#is_integer.trigger,
    TargetPath = Record#is_integer.target,
    Text = wf:js_escape(Record#is_integer.text),
    CustomValidatorAction =  #custom { trigger=TriggerPath, target=TargetPath, function=fun validate/2, text = Text, tag=Record },
    Script = wf:f("v.add(Validate.Numericality, { notAnIntegerMessage: \"~s\", onlyInteger: true });", [Text]),
    [CustomValidatorAction, Script].
    

validate(_, Value) -> 
    try is_integer(list_to_integer(Value)) == true
    catch _ : _ -> false
    end.
