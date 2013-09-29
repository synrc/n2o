-module (wf_validation).
-author('Rusty Klophaus').
-include_lib("n2o/include/wf.hrl").
-export ([validate/0]).

validate() ->
    ValidationGroup = wf_context:event_validation_group(),
    Validators = state_handler:get_state(validators, []),
    Validators1 = [X || X={VG, _, _} <- Validators, ValidationGroup == VG],
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
