% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_set).
-include_lib ("wf.hrl").
-compile(export_all).

% This action is used internally by Nitrogen.
render_action(Record) ->
    Anchor = Record#set.anchor,
    Target = Record#set.target,
    Value = wf:js_escape(wf:to_list(Record#set.value)),
    wf:f("Nitrogen.$set_value('~s', '~s', \"~s\");", [Anchor, Target, Value]).

set(Element, Value) ->
    wf:wire(Element, #set { value=Value }).


% TODO - not sure if anyone uses this anymore.
% bind(T, Record) when is_tuple(T), is_tuple(Record) -> 
% 	bind(2, T, Record).
% 	
% bind(Pos, T, Record) when Pos =< size(T) ->
% 	case element(Pos, T) of 
% 		undefined -> 
% 			ok;
% 		Element ->
% 			set(Element, element(Pos, Record))
% 	end,
% 	bind(Pos + 1, T, Record);
% 	
% bind(_, _, _) -> ok.
% 
% 
% reverse_bind(T) when is_tuple(T) ->
% 	Head = element(1, T),
% 	Tail = lists:duplicate(size(T) - 1, undefined),
% 	Record = list_to_tuple([Head|Tail]),
% 	reverse_bind(T, Record).
% 	
% reverse_bind(T, Record) when is_tuple(T), is_tuple(Record) -> 
% 	reverse_bind(2, T, Record). 
% 
% reverse_bind(Pos, T, Record) when Pos =< size(T) ->
% 	Value = case element(Pos, T) of
% 		undefined -> 
% 			element(Pos, Record);
% 		Element ->
% 			[X] = wf:q(Element),
% 			X
% 	end,
% 	reverse_bind(Pos + 1, T, setelement(Pos, Record, Value));
% 	
% reverse_bind(_, _, Record) -> Record.
