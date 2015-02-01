-module(action_transfer).
-author('Andrey Martemyanov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    case Record#transfer.state of
        undefined -> ok;
        List when is_list(List) -> [ erlang:put(K,V) || {K,V} <- List ];
        Single -> erlang:put(state,Single) end,
    Events = case Record#transfer.events of E when is_list(E) -> E; E -> [E] end,
    [ self() ! M || M <- Events ], ok.
