-module (wf_core).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

run() ->
    Request = wf_context:request_bridge(),
    Response = wf_context:response_bridge(),
    error_logger:info_msg("Request: ~p",[Request]),
    error_logger:info_msg("Response: ~p",[Response]),
    error_logger:info_msg("Context: ~p",[wf_context:context()]),
%    deserialize_context(),
    call_init_on_handlers(),
    wf_event:update_context_with_event(),
    Module = wf_context:event_module(),
    error_logger:info_msg("Module: ~p~n",[Module]),
    {module, Module} = code:ensure_loaded(Module),
    Data = Module:main(),                             % call page constructor
    error_logger:info_msg("Data: ~p~n",[Data]),
    wf_context:data(Data),
    Elements = wf_context:data(),
    wf_context:clear_data(),
    Actions = wf_context:actions(),
    wf_context:clear_actions(),
    {ok, Html1, Javascript1} = wf_render:render(Elements, Actions, undefined, undefined),
    element_flash:update(),
    ActionsFlash = wf_context:actions(),
    wf_context:clear_actions(),
    {ok, Html2, Javascript2} = wf_render:render([], ActionsFlash, undefined, undefined),
    call_finish_on_handlers(),
    Html = replace_script([Javascript1 ++ Javascript2], Html1 ++ Html2),
    Response2 = wf_context:response_bridge(), 
    Response1 = Response2:data(Html),
    error_logger:info_msg("Response: ~p~n",[Response1]),
    Response1:build_response().

call_init_on_handlers() -> [wf_handler:call(X#handler_context.name, init) || X <- wf_context:handlers()].
call_finish_on_handlers() -> [wf_handler:call(X#handler_context.name, finish) || X <- wf_context:handlers()].

replace_script(_,Html) when ?IS_STRING(Html) -> Html;
replace_script(Script, [script|T]) -> [Script|T];
replace_script(Script, [mobile_script|T]) -> [wf:html_encode(lists:flatten(Script))|T];
replace_script(Script, [H|T]) -> [replace_script(Script, H)|replace_script(Script, T)];
replace_script(_, Other) -> Other.
