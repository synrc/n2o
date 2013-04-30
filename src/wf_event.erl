-module(wf_event).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

update_context_with_event() ->
    PageModule = wf_context:page_module(),
    update_context_for_first_request().

update_context_for_first_request() ->
    Module = wf_context:page_module(),
    wf_context:event_module(Module),
    ok.

generate_postback_script(undefined, _Anchor, _ValidationGroup, _Delegate, _ExtraParam, Data) -> [];
generate_postback_script(Tag, Anchor, ValidationGroup, Delegate, ExtraParam, Data) ->
    Pickled = serialize_event_context(Tag, Anchor, ValidationGroup, Delegate),
    wf:f("ws.send(Bert.encodebuf({source: Bert.binary('~s'), "
                                 "pickle: Bert.atom('~s'), "
                                 "linked: ~s}));",[ValidationGroup,Pickled,Data]).

serialize_event_context(Tag, Anchor, ValidationGroup, Delegate) ->
    error_logger:info_msg("Serialized: ~p",[{Tag, Anchor, ValidationGroup, Delegate}]),
    PageModule = wf_context:page_module(),
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #event_context {
        module = EventModule,
        tag = Tag,
        anchor = Anchor,
        validation_group = ValidationGroup
    },
    wf_pickle:pickle(Event).
