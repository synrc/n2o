% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_redirect).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record=#redirect{nodrop=false}) ->
    DestinationUrl = Record#redirect.url,
    wf:f("window.location=\"~s\";", [wf:js_escape(DestinationUrl)]);

render_action(Record=#redirect{nodrop=true}) ->
    {ok, Html} = wf_render_elements:render_elements(#template{file=code:priv_dir(web) ++ "/templates/" ++ Record#redirect.url}),
    Re = re:replace(lists:flatten(Html),"\n"," ",[global,{return,list}]),
    error_logger:info_msg("Html: ~p",[Re]),
    wf:f("$('body').html('~s');", [Re]).

redirect(Url) -> 
    wf:wire(#redirect { url=Url }),
    wf:f("<script>window.location=\"~s\";</script>", [wf:js_escape(Url)]).

redirect_nodrop(Page) -> 
    wf_context:add_action(#redirect{url =  Page,nodrop=true}).

redirect_to_login(LoginUrl) ->
    % Assemble the original
    Request = wf_context:request_bridge(),
    OriginalURI = Request:uri(),
    PickledURI = wf:pickle(OriginalURI),
    redirect(LoginUrl ++ "?x=" ++ wf:to_list(PickledURI)).

redirect_from_login(DefaultUrl) ->	
    PickledURI = wf:q(x),
    case wf:depickle(PickledURI) of
        undefined -> redirect(DefaultUrl);
        Other -> redirect(Other)
    end.
