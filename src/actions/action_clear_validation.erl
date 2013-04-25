% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_clear_validation).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) -> 
    % Some values...
    TriggerPath = wf_render_actions:normalize_path(Record#clear_validation.validation_trigger),
    TargetPath = wf_render_actions:normalize_path(Record#clear_validation.validation_target),
	BothPath = wf_render_actions:normalize_path(Record#clear_validation.validation_all),

	if
		TriggerPath == undefined andalso TargetPath == undefined andalso BothPath == undefined ->
			%% nothing defined, so let's clear all validations
			clear_all_validators();
		BothPath =/= undefined ->
			%% If Let's just wipe out any validators at all related to the defined elements
			clear_trigger_validators(BothPath),
			clear_target_validators(BothPath);
		TriggerPath =/= undefined andalso TargetPath =/= undefined ->
			%% clear a validators on specific targets performed by specific triggers
			clear_specific_validators(TriggerPath, TargetPath);
		TriggerPath =/= undefined ->
			%% Clear validators triggered by a specific button
			clear_trigger_validators(TriggerPath);
		TargetPath =/= undefined ->
			%% Clear all validators assigned to specific fields
			clear_target_validators(TargetPath);
		true -> throw({action_clear_validation,neither_trigger_nor_target_defined})
	end.

clear_all_validators() ->
	set_validators([]),
	"Nitrogen.$destroy_all_validation()".

clear_specific_validators(Trigger, Target) ->
	Validators = get_validators(),
	FilteredValidators = [X || X={ValGroup, ValPath, _} <- Validators, not(ValGroup==Trigger andalso ValPath==Target)],
	set_validators(FilteredValidators),
	wf:f("Nitrogen.$destroy_specific_validation('~s','~s')",[Trigger, Target]).

clear_target_validators(Target) ->
	Validators = get_validators(),
	FilteredValidators = [X || X={_, ValPath, _} <- Validators, ValPath =/= Target],
	set_validators(FilteredValidators),
	wf:f("Nitrogen.$destroy_target_validation('~s')",[Target]).
	
clear_trigger_validators(Trigger) ->
	Validators = get_validators(),
	FilteredValidators = [X || X={ValGroup, _, _} <- Validators, ValGroup =/= Trigger],
	set_validators(FilteredValidators),
	wf:f("Nitrogen.$destroy_validation_group('~s')",[Trigger]).


get_validators() ->
	state_handler:get_state(validators, []).

set_validators(Validators) ->
	state_handler:set_state(validators, Validators).
