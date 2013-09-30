-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(undefined, _Anchor, _ValidationGroup, _Delegate, EventName, Data) -> [];
new(Tag, Anchor, ValidationGroup, Delegate, EventName, Data) ->
    Pickled = encode_ev(Tag, ValidationGroup, Delegate, EventName),
    wf:f("ws.send(Bert.encodebuf({source: Bert.binary('~s'), "
                                 "pickle: Bert.binary('~s'), "
                                 "linked: ~s}));",[ValidationGroup,Pickled,Data]).

encode_ev(Postback, ElementId, Delegate, Name) ->
    PageModule = ?CTX#context.module,
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #ev {
        name = Name,
        module = EventModule,
        payload = Postback,
        trigger = ElementId
    },
    wf_pickle:pickle(Event).
