-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(binary,Data) ->
    wf:f("ws.send(enc(tuple(atom('binary'),bin('~s'))));",[wf:pickle(Data)]).

new(undefined, _, _, _, _) -> [];
new(Postback, Element, Delegate, Name, Data) ->
    Module = wf:coalesce([Delegate, ?CTX#context.module]),
    Event = #ev{name=Name, module=Module, payload=Postback, trigger=Element},
    wf:f("ws.send(enc(tuple(atom('pickle'),bin('~s'),bin('~s'),~s)));",
        [Element,wf:pickle(Event),Data]).

