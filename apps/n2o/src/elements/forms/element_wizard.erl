% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_wizard).
-include ("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, wizard).

render_element(Record = #wizard{}) -> 
	% Set up callbacks...
	Tag = Record#wizard.tag,

	%ControlID = wf:temp_id(),

	% Set up steps...
	wf:assert(Record#wizard.id /= undefined, wizard_needs_a_proper_name),
	wf:assert(is_list(Record#wizard.steps), wizard_steps_must_be_a_list),
	wf:assert(Record#wizard.titles == undefined orelse length(Record#wizard.titles) == length(Record#wizard.steps), wizard_incorrect_number_of_titles),
	StepCount = length(Record#wizard.steps),
	StepSeq = lists:seq(1, StepCount),
	StepIDs = [wf:to_atom("step" ++ integer_to_list(X)) || X <- StepSeq],
	
	% Function to create an individual step.
	F = fun(N) ->
		StepID = lists:nth(N, StepIDs),
		StepTitle = case Record#wizard.titles of
			undefined -> "";
			_ -> lists:nth(N, Record#wizard.titles)
		end,
		StepBody = lists:nth(N, Record#wizard.steps),
		IsFirst = (N == 1),
		IsLast = (N == StepCount),

		ButtonRow = fun(TopOrBot) ->
			#panel{class=[wizard_buttons,combine(TopOrBot,wizard_buttons)],body=[
				#singlerow{cells=[
					#tablecell{class=wizard_buttons_back,body=[
						#button{
							id=combine(TopOrBot,back,N),
							show_if=(not IsFirst),
							text=Record#wizard.back,
							postback={back, N, StepIDs},
							delegate=?MODULE
						}
					]},
					#tablecell{class=wizard_buttons_next,body=[
						#button{
							id=combine(TopOrBot,next,N), 
							show_if=(not IsLast), 
							text=Record#wizard.next, 
							postback={next, N, StepIDs}, 
							delegate=?MODULE 
						},
						#button{
							id=combine(TopOrBot,finish,N), 
							show_if=IsLast, 
							text=Record#wizard.finish, 
							postback={finish, Tag}, 
							delegate=?MODULE 
						} 
					]}
				]}
			]}
		end,
		
		#panel { id=StepID, style="display: none;", body=[
			#panel{class=wizard_title,body=[
				#span{class=wizard_title_text,text=StepTitle},
				#span{class=wizard_progress,show_if=Record#wizard.show_progress,text=[
					"(",
					Record#wizard.progress_step,
					wf:to_list(N),
					Record#wizard.progress_of,
					wf:to_list(StepCount),
					")"
				]}
			]},
			ButtonRow(top),
			#panel { class=wizard_body, body=StepBody },
			ButtonRow(bottom)
		]}
	end,

	% Combine the steps.
	Terms = #panel {
		class=[wizard,Record#wizard.class],
		body=[F(X) || X <- StepSeq] 
	},
	
	% Show the first step.
	wf:wire(hd(StepIDs), #show{}),	
	
	% Render.
	Terms.
	
event({back, N, StepIDs}) -> 
	wf:wire(lists:nth(N, StepIDs), #hide {}),
	wf:wire(lists:nth(N - 1, StepIDs), #show {}),
	ok;	

event({next, N, StepIDs}) -> 
	wf:wire(lists:nth(N, StepIDs), #hide {}),
	wf:wire(lists:nth(N + 1, StepIDs), #show {}),
	ok;	

event({finish, Tag}) -> 
	Delegate = wf:page_module(),
	Delegate:wizard_event(Tag),
	ok;
	
event(_) -> ok.

combine(A, B) ->
	wf:to_atom(wf:to_list(A) ++ "_" ++ wf:to_list(B)).

combine(A, B, Step) ->
	wf:to_atom(wf:to_list(A) ++ "_" ++ wf:to_list(B) ++ "_" ++ wf:to_list(Step)).
