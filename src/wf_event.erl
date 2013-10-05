-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(undefined, _, _, _, _) -> [];
new(Postback, Element, Delegate, Name, Data) ->
    Module = wf:coalesce([Delegate, ?CTX#context.module]),
    Event = #ev{name=Name, module=Module, payload=Postback, trigger=Element},
    wf:f("ws.send(Bert.encodebuf("
         "{source: Bert.binary('~s'),"
          "pickle: Bert.binary('~s'),"
          "linked: ~s}));",[Element,wf:pickle(Event),Data]).
