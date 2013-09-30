-module(action_redirect).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record=#redirect{nodrop=false}) ->
    DestinationUrl = Record#redirect.url,
    wf:f("window.location=\"~s\";", [wf:js_escape(DestinationUrl)]);

render_action(Record=#redirect{nodrop=true}) ->
    Html = wf_core:render_item(#dtl{file=Record#redirect.url,bindings=[{hello,"Hello"}]}),
    Re = re:replace(lists:flatten(Html),"\n"," ",[global,{return,list}]),
    error_logger:info_msg("Html: ~p",[Re]),
    wf:f("$('body').html('~s');", [Re]).

redirect(Url) -> wf:wire(#redirect { url=Url }).

