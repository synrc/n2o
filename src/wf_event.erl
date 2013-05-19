-module(wf_event).
-author('Rusty Klophaus').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

generate_postback_script(undefined, _Anchor, _ValidationGroup, _Delegate, _ExtraParam, Data) -> [];
generate_postback_script(Tag, Anchor, ValidationGroup, Delegate, ExtraParam, Data) ->
    Pickled = serialize_event_context(Tag, Anchor, ValidationGroup, Delegate),
    wf:f("ws.send(Bert.encodebuf({source: Bert.binary('~s'), "
                                 "pickle: Bert.binary('~s'), "
                                 "extras: Bert.binary('~s'),"
                                 "linked: ~s}));",[ValidationGroup,Pickled,ExtraParam,Data]).

serialize_event_context(Tag, Anchor, ValidationGroup, Delegate) ->
%    error_logger:info_msg("Serialized: ~p",[{Tag, Anchor, ValidationGroup, Delegate}]),
    PageModule = wf_context:page_module(),
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #ev {
        module = EventModule,
        payload = Tag,
        trigger = ValidationGroup
    },
    wf_pickle:pickle(Event).