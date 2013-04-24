-module (wf_core).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

run() ->
    call_init_on_handlers(),
    wf_event:update_context_with_event(),
    Module = wf_context:event_module(),
    Elements = Module:main(),
    Actions = wf_context:actions(),
    {ok, Html, JavaScript} = render(Elements, Actions, undefined, undefined),
    call_finish_on_handlers(),
    HtmlJS = replace_script([JavaScript], Html),
    ResponseBridge = wf_context:response_bridge(), 
    Response = ResponseBridge:data(HtmlJS),
    Response:build_response().

render(Elements, Actions, Trigger, Target) ->
    {ok, Html}    = wf_render_elements:render_elements(Elements),
    {ok, Script1} = wf_render_actions:render_actions(Actions, Trigger, Target),
    QueuedActions = wf_context:actions(),
    {ok, Script2} = wf_render_actions:render_actions(QueuedActions, Trigger, Target),
    Script= [Script1, Script2],
    {ok, Html, Script}.

call_init_on_handlers() -> [wf_handler:call(X#handler_context.name, init) || X <- wf_context:handlers()].
call_finish_on_handlers() -> [wf_handler:call(X#handler_context.name, finish) || X <- wf_context:handlers()].

replace_script(_,Html) when ?IS_STRING(Html) -> Html;
replace_script(Script, [script|T]) -> [Script|T];
replace_script(Script, [mobile_script|T]) -> [wf:html_encode(lists:flatten(Script))|T];
replace_script(Script, [H|T]) -> [replace_script(Script, H)|replace_script(Script, T)];
replace_script(_, Other) -> Other.
