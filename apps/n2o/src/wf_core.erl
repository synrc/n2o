-module (wf_core).
-include_lib ("wf.hrl").
-compile (export_all).

run() ->
    Request = wf_context:request_bridge(),
    Response = wf_context:response_bridge(),
%    deserialize_context(),
    call_init_on_handlers(),
    wf_event:update_context_with_event(),
    Module = wf_context:event_module(),
    error_logger:info_msg("Module: ~p~n",[Module]),
    {module, Module} = code:ensure_loaded(Module),
    Data = Module:main(),
    error_logger:info_msg("Data: ~p~n",[Data]),
    wf_context:data(Data),
    Elements = wf_context:data(),
    wf_context:clear_data(),
    Actions = wf_context:actions(),
    wf_context:clear_actions(),
    {ok, Html1, Javascript1} = wf_render:render(Elements, Actions, undefined, undefined, undefined),
    element_flash:update(),
    ActionsFlash = wf_context:actions(),
    wf_context:clear_actions(),
    {ok, Html2, Javascript2} = wf_render:render([], ActionsFlash, undefined, undefined, undefined),
%    call_finish_on_handlers(),
    StateScript = serialize_context(),
    Html = replace_script([StateScript, Javascript1 ++ Javascript2], Html1 ++ Html2),
    Response2 = wf_context:response_bridge(), 
    Response1 = Response2:data(Html),
    error_logger:info_msg("Response: ~p~n",[Response1]),
    Response1:build_response().

run2() ->
    Request = wf_context:request_bridge(),
    Response = wf_context:response_bridge(),
    try 
        case Request:error() of
            none -> run_catched();
            Other -> 
                Message = wf:f("Errors: ~p~n", [Other]),
                Response1 = Response:data(Message),
                Response1:build_response()
        end
    catch Type : Error -> 
        ?LOG("~p~n", [{error, Type, Error, erlang:get_stacktrace()}]),
        ErrResponse = Response:status_code(500),
        ErrResponse1 = ErrResponse:data("Internal Server Error"),
        ErrResponse1:build_response()
    end.

run_catched() ->
    % Get the handlers from querystring, if they exist...
    deserialize_context(),

    % Initialize all handlers...
    call_init_on_handlers(),

    % Deserialize the event if available...
    wf_event:update_context_with_event(),

    % TODO - Check for access

    % Call the module...
    case wf_context:type() of
        first_request    -> 
            run_first_request(), 
            finish_dynamic_request();
        postback_request -> 
            run_postback_request(), 
            finish_dynamic_request();
        static_file      -> 
            finish_static_request()
    end.

finish_dynamic_request() ->
    % Get elements and actions...
    Elements = wf_context:data(),
    wf_context:clear_data(),

    Actions = wf_context:actions(),
    wf_context:clear_actions(),

    % Render...
    {ok, Html1, Javascript1} = wf_render:render(Elements, Actions, undefined, undefined, undefined),

    % Update flash and render
    % Has to be here because has_flash state is not set before render
    element_flash:update(),	
    ActionsFlash = wf_context:actions(),
    wf_context:clear_actions(),
    {ok, Html2, Javascript2} = wf_render:render([], ActionsFlash, undefined, undefined, undefined),

%%	%% Like the flash stuff, we have to load the deferred actions last, since
	%% in an offensively non-functional way, some elements and actions will be
	%% deferred when rendered above
%%	ActionsDeferred = wf_context:deferred(),
%%	wf_contxt:clear_deferred(),
%%	{ok, _, Javascript3} = wf_render:render([], ActionsDeferred, undefined, undefined, undefined),


    % Call finish on all handlers.
    call_finish_on_handlers(),

    % Create Javascript to set the state...
    StateScript = serialize_context(),
    JavascriptFinal = [StateScript, Javascript1 ++ Javascript2],
    case wf_context:type() of
        first_request       -> build_first_response(Html1 ++ Html2, JavascriptFinal);
        postback_request    -> build_postback_response(JavascriptFinal)
    end.

finish_static_request() ->
    Path = wf_context:path_info(),
    build_static_file_response(Path).

%%% SERIALIZE AND DESERIALIZE STATE %%%

% serialize_context_state/0 -
% Serialize part of Context and send it to the browser
% as Javascript variables.
serialize_context() ->
    % Get page context...
    Page = wf_context:page_context(),

    % Get handler context, but don't serialize the config.
    Handlers = [X#handler_context { config=undefined } || X <- wf_context:handlers()],
    SerializedContextState = wf_pickle:pickle([Page, Handlers]),
    wf:f("Nitrogen.$set_param('pageContext', '~s');~n", [SerializedContextState]).

% deserialize_context_state/1 -
% Updates the context with values that were stored
% in the browser by serialize_context_state/1.
deserialize_context() ->
    RequestBridge = wf_context:request_bridge(),	
    Params = RequestBridge:post_params(),

    % Save the old handles...
    OldHandlers = wf_context:handlers(),

    % Deserialize page_context and handler_list if available...
    SerializedPageContext = proplists:get_value("pageContext", Params),
    [Page, Handlers] = case SerializedPageContext of
        undefined -> [wf_context:page_context(), wf_context:handlers()];
        Other -> wf_pickle:depickle(Other)
    end,

    % Config is not serialized, so copy config from old handler list to new
    % handler list.
    Handlers1 = copy_handler_config(OldHandlers, Handlers),

    % Create a new context...
    wf_context:page_context(Page),
    wf_context:handlers(Handlers1),

    % Return the new context...
    ok.

copy_handler_config([], []) -> [];
copy_handler_config([H1|T1], [H2|T2]) when H1#handler_context.name == H2#handler_context.name ->
    [H2#handler_context { config=H1#handler_context.config }|copy_handler_config(T1, T2)];
copy_handler_config(L1, L2) -> 
    ?PRINT(L1),
    ?PRINT(L2),
    throw({?MODULE, handler_list_has_changed}).

%%% SET UP AND TEAR DOWN HANDLERS %%%

% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
call_init_on_handlers() ->
    Handlers = wf_context:handlers(),
    [wf_handler:call(X#handler_context.name, init) || X <- Handlers],
    ok.

% finish_handlers/1 - 
% Handlers are finished in the order that they exist in #context.handlers. The order
% is important, as some handlers should finish after others. At the very least,
% the 'render' handler should go last to make sure that it captures all changes
% put in place by the other handlers.
call_finish_on_handlers() ->
    Handlers = wf_context:handlers(),
    [wf_handler:call(X#handler_context.name, finish) || X <- Handlers],
    ok.	


%%% FIRST REQUEST %%%

run_first_request() ->
    % Some values...
    Module = wf_context:event_module(),
    {module, Module} = code:ensure_loaded(Module),
    Data = Module:main(),
    wf_context:data(Data).


%%% POSTBACK REQUEST %%%

run_postback_request() ->
    % Some values...
    Module = wf_context:event_module(),
    Tag = wf_context:event_tag(),

    % Validate...
    {ok, IsValid} = wf_validation:validate(),

    % Call the event...
    case IsValid of
        true -> Module:event(Tag);
        false -> ok
    end.

%%% BUILD THE RESPONSE %%%

build_static_file_response(Path) ->
    Response = wf_context:response_bridge(),
    Response1 = Response:file(Path),
    Response1:build_response().

build_first_response(Html, Script) ->
    % Update the output with any script...
    Html1 = replace_script(Script, Html),

    % Update the response bridge and return.
    Response = wf_context:response_bridge(),
    Response1 = Response:data(Html1),
    Response1:build_response().

build_postback_response(Script) ->
    % Update the response bridge and return.
    Response = wf_context:response_bridge(),
    % TODO - does this need to be flattened?
    Response1 = Response:data(lists:flatten(Script)),
    Response1:build_response().

replace_script(_,Html) when ?IS_STRING(Html) -> Html;
replace_script(Script, [script|T]) -> [Script|T];
%% For the mobile_script, it's necessary that it's inside the data-role attr,
%% and therefore must be escaped before it can be sent to the browser
replace_script(Script, [mobile_script|T]) -> [wf:html_encode(lists:flatten(Script))|T];
replace_script(Script, [H|T]) -> [replace_script(Script, H)|replace_script(Script, T)];
replace_script(_, Other) -> Other.
