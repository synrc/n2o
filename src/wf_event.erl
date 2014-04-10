-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(undefined, _, _, _, _) -> [];
new(Postback, Element, Delegate, Name, Data) ->
    Module = wf:coalesce([Delegate, ?CTX#context.module]),
    Event = #ev{name=Name, module=Module, payload=Postback, trigger=Element},
    wf:f("ws.send(bert.encodebuf(bert.tuple(bert.atom('wf_event'),"
         "bert.binary('~s'),bert.binary('~s'),~s)));",[Element,wf:pickle(Event),Data]).
