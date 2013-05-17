-module(wf_event).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

generate_postback_script(undefined, _Anchor, _ValidationGroup, _Delegate, _ExtraParam, Data) -> [];
generate_postback_script(Tag, Anchor, ValidationGroup, Delegate, ExtraParam, Data) ->
    Pickled = serialize_event_context(Tag, Anchor, ValidationGroup, Delegate),
    wf:f("ws.send(Bert.encodebuf({source: Bert.binary('~s'), "
                                 "pickle: Bert.binary('~s'), "
                                 "extras: Bert.binary('~s'),"
                                 "linked: ~s}));",[ValidationGroup,Pickled,ExtraParam,Data]).

jsonx_encoder() -> jsonx:encoder([{event_context,record_info(fields,event_context)},
                                  {api_event,record_info(fields,api_event)},
                                  {api,record_info(fields,api)}
                                 ]).

jsonx_decoder() -> jsonx:decoder([{event_context,record_info(fields,event_context)},
                                  {api_event,record_info(fields,api_event)},
                                  {api,record_info(fields,api)}
                                 ]).

serialize_event_context(Tag, Anchor, ValidationGroup, Delegate) ->
%    error_logger:info_msg("Serialized: ~p",[{Tag, Anchor, ValidationGroup, Delegate}]),
    PageModule = get(page_module),
    EventModule = wf:coalesce([Delegate, PageModule]),
%    Event =  [ wf:to_list(EventModule),"~",
%              lists:flatten(io_lib:format("~p",[Tag])),"~",
%              wf:to_list(Anchor), "~",
%              wf:to_list(ValidationGroup) ],
    Event = #event_context {
        module = EventModule,
        tag = Tag,
        anchor = Anchor,
        validation_group = ValidationGroup
    },
    wf_pickle:pickle(Event).
%    Encoder = get(encoder), %jsonx_encoder(),
%    Json = Encoder(Event),
%    error_logger:info_msg("JSON: ~p",[Json]),
%    Json.
