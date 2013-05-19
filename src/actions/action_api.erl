-module(action_api).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
%    error_logger:info_msg("Render API"),
    Anchor = Record#api.anchor,
    Name = Record#api.name,
    Tag = #ev{payload=Record},
    Data = "Bert.encode(event)",
    PostbackScript = wf_event:generate_postback_script(Tag, Anchor, "document", undefined, api, Data),
    wf:f("document.~s = function anonymous(event) { ", [Name]) ++ PostbackScript ++ "};".

event(#ev{payload=Record},Args,Ctx) ->
    error_logger:info_msg("API CALL ~p",[Record]),
    Module = wf:coalesce([Record#api.delegate, Ctx#context.module]),
    Term = binary_to_term(list_to_binary(Args)),
    Module:api_event(Record#api.name, Record#api.tag, Term).
