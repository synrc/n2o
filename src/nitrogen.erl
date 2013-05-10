-module(nitrogen).
-compile(export_all).

init_request(RequestBridge, ResponseBridge) -> wf_context:init_context(RequestBridge, ResponseBridge).
handler(Module, Config) -> wf_handler:set_handler(Module, Config).
run() -> wf_core:run().
handling_module() -> {ok, Root} = application:get_env(nitrogen_handler_module), Root.
