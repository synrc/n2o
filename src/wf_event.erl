-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

generate_postback_script(undefined, _Anchor, _ValidationGroup, _Delegate, EventName, Data) -> [];
generate_postback_script(Tag, Anchor, ValidationGroup, Delegate, EventName, Data) ->
    Pickled = serialize_event_context(Tag, ValidationGroup, Delegate, EventName),
    wf:f("ws.send(Bert.encodebuf({source: Bert.binary('~s'), "
                                 "pickle: Bert.binary('~s'), "
%                                 "extras: Bert.binary('~s'),"
                                 "linked: ~s}));",[ValidationGroup,Pickled,Data]).

serialize_event_context(Postback, ElementId, Delegate, Name) ->
    PageModule = wf_context:page_module(),
%    error_logger:info_msg("Serialized: ~p",[PageModule]),
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #ev {
        name = Name,
        module = EventModule,
        payload = Postback,
        trigger = ElementId
    },
    wf_pickle:pickle(Event).