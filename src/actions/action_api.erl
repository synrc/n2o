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
    PostbackScript = wf_event:generate_postback_script(Tag, Anchor, "document", undefined, api_event, Data),
    wf:f("document.~s = function anonymous(event) { ", [Name]) ++ PostbackScript ++ "};".

