-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(undefined, _Element, _Delegate, EventName, Data) -> [];
new(Postback, Element, Delegate, EventName, Data) ->
    Pickled = encode_ev(Postback, Element, Delegate, EventName),
    wf:f("ws.send(Bert.encodebuf({source: Bert.binary('~s'), "
                                 "pickle: Bert.binary('~s'), "
                                 "linked: ~s}));",[Element,Pickled,Data]).

encode_ev(Postback, Element, Delegate, Name) ->
    PageModule = ?CTX#context.module,
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #ev {
        name = Name,
        module = EventModule,
        payload = Postback,
        trigger = Element
    },
    wf:pickle(Event).
