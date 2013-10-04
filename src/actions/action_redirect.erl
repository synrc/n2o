-module(action_redirect).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record=#redirect{}) ->
    DestinationUrl = Record#redirect.url,
    wf:f("window.location=\"~s\";", [wf:js_escape(DestinationUrl)]);

