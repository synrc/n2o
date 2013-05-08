-module (dynamic_route_handler).
-behaviour (route_handler).
-include_lib ("wf.hrl").
-export ([init/2, finish/2]).

init(_Config, State) -> 
    RequestBridge = wf_context:request_bridge(),
    Path = RequestBridge:path(),
    {Module, PathInfo} = route(Path),
    {Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),
    wf_context:page_module(Module1),
    wf_context:path_info(PathInfo1),
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

route("/") -> {list_to_atom(module_name(["index"])), []};
route("/websocket") -> {list_to_atom(module_name(["index"])), []};

route(Path) ->
    IsStatic = (filename:extension(Path) /= []),
    case IsStatic of
        true ->
            % Serve this up as a static file.
            {static_file, Path};

        false ->
            Path1 = string:strip(Path, both, $/),
            Tokens = string:tokens(Path1, "/"),
            % Check for a loaded module. If not found, then try to load it.
            case try_load_module(Tokens) of
                {Module, PathInfo} -> 
                    {Module, PathInfo};
                undefined ->
                    {web_404, Path1}
            end
    end.

module_name(Tokens) ->
    ModulePrefix = wf:config_default(module_prefix, ""),
    AllTokens = case ModulePrefix of
                     "" -> Tokens;
                     _ -> [ ModulePrefix | Tokens ] end,
    _ModuleName = string:join(AllTokens, "_").

try_load_module(Tokens) -> try_load_module(Tokens, []).
try_load_module([], _ExtraTokens) -> undefined;
try_load_module(Tokens, ExtraTokens) ->
    %% Get the module name...
    ModuleName = module_name(Tokens),
    Module = try 
        list_to_existing_atom(ModuleName)
    catch _:_ ->
        case erl_prim_loader:get_file(ModuleName ++ ".beam") of
            {ok, _, _} -> list_to_atom(ModuleName);
            _ -> list_to_atom("$not_found")
        end
    end,

    %% Load the module, check if it exports the right method...
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, main, 0) of
        true -> 
            PathInfo = string:join(ExtraTokens, "/"),
            {Module, PathInfo};
        false ->
            next_try_load_module(Tokens, ExtraTokens)
    end.


next_try_load_module(Tokens, ExtraTokens) ->
    Tokens1 = lists:reverse(tl(lists:reverse(Tokens))),
    ExtraTokens1 = [hd(lists:reverse(Tokens))|ExtraTokens],
    try_load_module(Tokens1, ExtraTokens1).

check_for_404(static_file, _PathInfo, Path) ->
    {static_file, Path};

check_for_404(Module, PathInfo, Path) ->
%    error_logger:info_msg("Module Path ~p ~p",[Module, Path]),
    case code:ensure_loaded(Module) of
        {module, Module} -> {Module, PathInfo};
        _ -> 
            case code:ensure_loaded(web_404) of
                {module, web_404} -> {web_404, Path};
                _ -> {file_not_found_page, Path}
            end
    end.
