% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module (element_inplace).
-include_lib("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, inplace).

render_element(#inplace{text=Text, tag=Tag, delegate=Delegate, edit=Edit,
		view=View, class=Class, style=Style, start_mode=StartMode}) ->

	OKButtonID = wf:temp_id(),
	CancelButtonID = wf:temp_id(),
	ViewPanelID = wf:temp_id(),
	EditPanelID = wf:temp_id(),

	% Use id of edit if set, otherwise generate one

	BaseEdit = wf_utils:get_elementbase(Edit),
	EditModule = BaseEdit#elementbase.module,
	EditID = wf:coalesce([BaseEdit#elementbase.id, wf:temp_id()]),

	% Use id of view if set, otherwise generate one

	BaseView = wf_utils:get_elementbase(View),
	ViewModule = BaseView#elementbase.module,
	ViewID = wf:coalesce([BaseView#elementbase.id, wf:temp_id()]),

	% Set up the events...

	Controls = {ViewPanelID, ViewID, EditPanelID, EditID},
	OKPostback = {ok, Delegate, Controls, Tag},
	CancelPostback = {cancel, Controls, Tag},
	CancelEvent = #event { delegate=?MODULE, postback=CancelPostback, actions=[
			#script {
				script = wf:f("v=Nitrogen.$get_value('~s', '~s');Nitrogen.$set_value('~s', '~s', v);",
					["", ViewID, "", EditID]) }
		]},

	% Create the view...

	ViewAction = [
		#buttonize { target=ViewPanelID }
	],

	View1 = wf_utils:replace_field(id, ViewID, ViewModule:reflect(), View),
	View2 = append_field_actions(ViewAction, undefined, ViewModule:reflect(), View1),
	View3 = replace_field_text(Text, View2, ViewModule:reflect()),

	% Create the edit...

	EditAction = [
		#event { type=enterkey, shift_key=false, actions=#script { script=["objs('", OKButtonID, "').click();"] } },
		#event { type=keyup, keycode=27, actions=#script { script=["objs('", CancelButtonID, "').click();"] } }
	],

	Edit1 = wf_utils:replace_field(id, EditID, EditModule:reflect(), Edit),
	Edit2 = append_field_actions(EditAction, OKButtonID, EditModule:reflect(), Edit1),
	Edit3 = replace_field_text(Text, Edit2, EditModule:reflect()),

	% No value in view mode cause the view panel unclickable thus
	% we set edit mode in that case.

	StartMode1 = case string:strip(Text) of
		[] -> edit;
		_ -> StartMode
	end,

	case StartMode1 of
		view ->
			wf:wire(EditPanelID, #hide{});
		edit ->
			wf:wire(ViewPanelID, #hide{}),
			Script = #script { script="objs('me').focus(); objs('me').select();" },
			wf:wire(EditID, Script)
	end,

	% Create the main panel...

	#panel { class=[inplace, Class], style=Style, body=[
			#panel { id=ViewPanelID, class="view", body=[ View3 ], actions = [
					#event { type=click, actions=[
							#hide { target=ViewPanelID },
							#show { target=EditPanelID },
							#script { script = wf:f("objs('~s').focus(); objs('~s').select();", [EditID, EditID]) }
						]}
				]},
			#panel { id=EditPanelID, class="edit", body=[
					Edit3,
					#button { id=OKButtonID, text="OK", delegate=?MODULE, postback=OKPostback },
					#button { id=CancelButtonID, text="Cancel", actions = [CancelEvent#event{ type=click }] }
				]}
		]
	}.

event({ok, Delegate, {ViewPanelID, ViewID, EditPanelID, EditID}, Tag}) ->
	Module = wf:coalesce([Delegate, wf:page_module()]),
	Value = Module:inplace_event(Tag, string:strip(wf:q(EditID))),
	wf:set(ViewID, Value),
	wf:set(EditID, Value),
	case Value =/= "" of
			true ->
				wf:wire(EditPanelID, #hide {}),
				wf:wire(ViewPanelID, #show {});
			false -> ok
	end;

event({cancel, {ViewPanelID, _ViewID, EditPanelID, EditID}, _Tag}) ->
	case string:strip(wf:q(EditID)) =/= "" of
			true ->
				wf:wire(EditPanelID, #hide {}),
				wf:wire(ViewPanelID, #show {});
			false -> ok
	end.

replace_field_text(Value, Element, Fields) ->
	case element(1, Element) of
		dropdown -> wf_utils:replace_field(value, Value, Fields, Element);
		image -> wf_utils:replace_field(image, Value, Fields, Element);
		_ -> wf_utils:replace_field(text, Value, Fields, Element)
	end.

modify_field_action(Action, Trigger) ->
	case element(1, Action) of
		validate -> Action#validate{ trigger = Trigger };
		_ -> Action
	end.

%% Set required trigger to original validate actions and append
%% new actions specified by Actions.
append_field_actions(Actions, Trigger, Fields, Rec) ->
	N = wf_utils:indexof(actions, Fields),
	ModifiedOldActions = case element(N, Rec) of
		undefined -> [];
		OldActions when is_list(OldActions) ->
			[modify_field_action(X, Trigger) || X <- OldActions];
		OldActions -> modify_field_action(OldActions, Trigger)
	end,
	setelement(N, Rec, Actions ++ ModifiedOldActions).

