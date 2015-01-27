-module(action_transfer).
-author('Andrey Martemyanov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
	case Record#transfer.state of undefined -> ok; State -> erlang:put(state,State) end,
	Events = case Record#transfer.events of E when is_list(E) -> E; E -> [E] end,
	[ self() ! M || M <- Events ], ok.
