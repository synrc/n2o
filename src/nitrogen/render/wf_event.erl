-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(bin,Data) ->
    wf:f("ws.send(enc(tuple(atom('bin'),bin('~s'))));",[wf:pickle(Data)]).

new(undefined, _, _, _, _) -> [];
new(Postback, Element, Delegate, Name, Data) ->
    Module = wf:coalesce([Delegate, ?CTX#context.module]),
    Event = #ev{name=Name, module=Module, payload=Postback, trigger=Element},
    wf:f("ws.send(enc(tuple(atom('~w'),bin('~s'),bin('~s'),~s)));",
        [wf:config(n2o,event,pickle),Element,wf:pickle(Event),Data]).
