% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_sparkline).
-include_lib ("wf.hrl").
-compile(export_all).

%% @doc
%% Usage: #sparkline { values=[1, 2, 5, 6, 10, 9] }
%% http://willarson.com/code/sparklines/sparklines.html

reflect() -> record_info(fields, sparkline).

render_element(Record) -> 
    Anchor = Record#sparkline.anchor,
    Values = Record#sparkline.values,
    ValuesS = "[" ++ string:join([wf:to_list(X) || X <- Values], ",") ++ "]",
    OptionsS = options_to_js([{type, Record#sparkline.type}|Record#sparkline.options]),
    wf:wire(wf:f("objs('~s').sparkline(~s, ~s);", [Anchor, ValuesS, OptionsS])),
    Span = #span {
        html_id = Record#sparkline.html_id,
	id = Record#sparkline.id,
	anchor = Record#sparkline.anchor,
	class = [sparkline, Record#sparkline.class],
	style = Record#sparkline.style
    },
    element_span:render_element(Span).


%% Options is a list of {Key,Value} tuples	
options_to_js(Options) ->
    F = fun({Key, Value}) ->
	if 
	    is_list(Value) -> 
		wf:f("~s: '~s'", [Key, wf:js_escape(Value)]);
	    is_atom(Value) andalso (Value == true orelse Value == false) ->
		wf:f("~s: ~s", [Key, Value]);
	    is_atom(Value) ->
		wf:f("~s: '~s'", [Key, Value]);
	    true -> 
		wf:f("~s: ~p", [Key, Value])
	end
    end,
    Options1 = [F(X) || X <- Options],
    Options2 = string:join(Options1, ","),
    wf:f("{ ~s }", [Options2]).
