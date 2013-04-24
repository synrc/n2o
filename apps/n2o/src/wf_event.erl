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
    wf_context:type(first_request),
    wf_context:anchor("page"),
    ok.

generate_postback_script(undefined, _Anchor, _ValidationGroup, _Delegate, _ExtraParam) -> [];
generate_postback_script(Postback, Anchor, ValidationGroup, Delegate, ExtraParam) ->
    PickledPostbackInfo = serialize_event_context(Postback, Anchor, ValidationGroup, Delegate),
    error_logger:info_msg("Postback: ~p~n",[{Postback, Anchor, ValidationGroup, Delegate, ExtraParam}]),
    wf:f("ori = Bert.encode({source: Bert.binary('~s'), pickle: Bert.binary('~s'), x: Bert.binary('~s'), module: Bert.atom('~s')});"
         "buf = new Uint8Array(new ArrayBuffer(ori.length));"
         "for (var i=0;i<buf.length;i++) { buf[i] = ori.charCodeAt(i); }"
         "ws.send(buf);",[ValidationGroup,PickledPostbackInfo, ExtraParam, wf_context:page_module()]).

generate_system_postback_script(undefined, _Anchor, _ValidationGroup, _Delegate) -> [];
generate_system_postback_script(Postback, Anchor, ValidationGroup, Delegate) ->
    PickledPostbackInfo = serialize_event_context(Postback, Anchor, ValidationGroup, Delegate),
    wf:f("Nitrogen.$queue_system_event('~s');", [PickledPostbackInfo]).

serialize_event_context(Tag, Anchor, ValidationGroup, Delegate) ->
    PageModule = wf_context:page_module(),
    EventModule = wf:coalesce([Delegate, PageModule]),
    Event = #event_context {
        module = EventModule,
        tag = Tag,
        anchor = Anchor,
        validation_group = ValidationGroup
    },
    wf_pickle:pickle(Event).
