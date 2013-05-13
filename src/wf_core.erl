-module (wf_core).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

run() ->
    call_init_on_handlers(),
    Module = wf_context:page_module(),
    Elements = Module:main(),
    Actions = wf_context:actions(),
    Script = wf_render_actions:render_actions(Actions, undefined, undefined),
    put(script,Script),
    Html = wf_render_elements:render_elements(Elements),
    call_finish_on_handlers(),
    ResponseBridge = wf_context:response_bridge(),
    Response = ResponseBridge:data(Html),
    Response:build_response().

call_init_on_handlers() -> [wf_handler:call(X#handler_context.name, init) || X <- wf_context:handlers()].
call_finish_on_handlers() -> [wf_handler:call(X#handler_context.name, finish) || X <- wf_context:handlers()].

