-module (dynamic_route_handler).
-author('Rusty Klophaus').
-behaviour (route_handler).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

init(State, Ctx) -> 
%    RequestBridge = wf_context:request_bridge(),
    Path = wf:path(Ctx#context.req), %RequestBridge:path(),
    {Module, PathInfo} = route(Path),
%    error_logger:info_msg("Module: ~p",[Module]),
%    error_logger:info_msg("Path: ~p",[Path]),
%    {Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),
%    wf_context:page_module(Module),
%    put(page_module,Module),
%    wf_context:path_info(PathInfo1),
    {ok, State, Ctx#context{path=PathInfo,module=Module}}.

finish(State, Ctx) -> 
    {ok, State, Ctx}.

route(<<"/">>) -> {index, []};
route(<<"/index">>) -> {index, []};
route(<<"/hello">>) -> {hello, []};
route(<<"/websocket/">>) -> {index, []};
route(<<"/websocket/index">>) -> {index, []};
route(<<"/websocket/hello">>) -> {hello, []};
route(<<"/favicon.ico">>) -> {static_file, []}.

