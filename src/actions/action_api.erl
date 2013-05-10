-module(action_api).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    Anchor = Record#api.anchor,
    Name = Record#api.name,
    Tag = {api_event, Record},
    Data = "Bert.encode(event)",
    PostbackScript = wf_event:generate_postback_script(Tag, Anchor, "document", undefined, api, Data),
    [
        wf:f("document.~s = function anonymous(event) { ", [Name]), PostbackScript, "};"
    ].

event({api_event, Record},Args) ->
    error_logger:info_msg("API CALL"),
    Module = wf:coalesce([Record#api.delegate, wf_context:page_module()]),
    Term = binary_to_term(list_to_binary(Args)),
    Module:api_event(Record#api.name, Record#api.tag, Term).
