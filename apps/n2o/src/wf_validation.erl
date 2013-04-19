% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_validation).
-include_lib ("wf.hrl").
-export ([validate/0]).

validate() ->
    % Some values...
    ValidationGroup = wf_context:event_validation_group(),
    Validators = state_handler:get_state(validators, []),

    % Get all validators that match the validation group.
    % ValidationGroup is a string.
    Validators1 = [X || X={VG, _, _} <- Validators, ValidationGroup == VG],

    % Now, run through each matching validator.
    % Stop validating a TargetPath when it has failed.
    F2 = fun({_, TargetPath, Record}, FailedPaths) ->
        case lists:member(TargetPath, FailedPaths) of
            true -> 
                FailedPaths;
            false ->
                Function = Record#custom.function,
                Text = Record#custom.text,
                Value = wf:q(TargetPath),
                case Function(Record#custom.tag, Value) of
                    true -> 
                        FailedPaths;
                    false ->
                        wf:wire(TargetPath, #validation_error { text=Text }),
                        [TargetPath|FailedPaths]
                end
        end
    end,

    FailedPaths1 = lists:foldl(F2, [], Validators1),
    {ok, FailedPaths1 == []}.
