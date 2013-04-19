% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (action_api).
-include_lib ("wf.hrl").
-compile(export_all).

render_action(Record) ->
    Anchor = Record#api.anchor,
    Name = Record#api.name,
    Tag = {api_event, Record},
    [
        wf:f("obj('~s').~s = function() {", [Anchor, Name]),
        "var s = Nitrogen.$encode_arguments_object(arguments);",
        #event { postback=Tag, delegate=?MODULE, extra_param="s" },
        "};"
    ].

event({api_event, Record}) ->
    Module = wf:coalesce([Record#api.delegate, wf_context:page_module()]),
    Args = wf:q(args),
    Term = binary_to_term(list_to_binary(Args)),
    Module:api_event(Record#api.name, Record#api.tag, Term).
