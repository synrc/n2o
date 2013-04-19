% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (dynamic_route_handler).
-behaviour (route_handler).
-include_lib ("wf.hrl").
-export ([
    init/2, 
    finish/2
]).

%% @doc
%% dynamic_route_handler looks at the requested path and file extension
%% to determine how a request should be served. If the request path has no 
%% extension, then it assumes this request should be handled by a Nitrogen
%% page module that matches the path with slashes converted to underscores.
%% If no module is found, then it will chop off the last part of the path 
%% (storing it for later access in wf:path_info/0) and try again, repeating
%% until either a module is found, or there are no more parts to chop. If
%% a module still can't be found, then the web_404 module is used if defined
%% by the user, otherwise a 404 is generated internally.
%% 
%% Requests for "/" are automatically sent to index.
%%
%% If the request path does have an extension, then it is treated like a request
%% for a static file. This is delegated back to the HTTP server.

init(_Config, State) -> 
    % Get the path...
    RequestBridge = wf_context:request_bridge(),
    Path = RequestBridge:path(),

    % Convert the path to a module. If there are no routes defined, then just
    % convert everything without an extension to a module.
    % Otherwise, look through all routes for the first matching route.
    {Module, PathInfo} = route(Path),
    {Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),

    wf_context:page_module(Module1),
    wf_context:path_info(PathInfo1),

    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

%%% PRIVATE FUNCTIONS %%%

% First, check if this is a request for the root path. If so
% then just send it to index.
% Check if there is an extension, if so, it's static.
% Otherwise, try to load a module according to each part of the path.
% First, cycle through code:all_loaded(). If not there, then check erl_prim_loader:get_file()
% If still not there, then 404.
route("/") -> 
    {list_to_atom(module_name(["index"])), []};

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

%% find_loaded_module(Tokens) -> find_loaded_module(Tokens, []).	
%% find_loaded_module([], _ExtraTokens) -> undefined;
%% find_loaded_module(Tokens, ExtraTokens) ->
%%     BeamFile = "/" ++ string:join(Tokens, "_") ++ ".beam",
%%     F = fun({_Module, Path}) -> is_list(Path) andalso string:rstr(Path, BeamFile) /= 0 end,
%%     case lists:filter(F, code:all_loaded()) of 
%%         [{Module, _}] -> 
%%             case erlang:function_exported(Module, main, 0) of
%%                 true ->
%%                     {Module, string:join(lists:reverse(ExtraTokens), "/")};
%%                 false ->
%%                     find_loaded_module(tl(lists:reverse(Tokens)), [hd(lists:reverse(Tokens))|ExtraTokens])
%%             end;
%%         [] -> 
%%             find_loaded_module(tl(lists:reverse(Tokens)), [hd(lists:reverse(Tokens))|ExtraTokens])
%%     end.

module_name(Tokens) ->
    ModulePrefix = wf:config_default(module_prefix, ""),
	AllTokens = case ModulePrefix of
	    "" -> Tokens;
	    _ -> [ ModulePrefix | Tokens ]
	end,
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
    % Make sure the requested module is loaded. If it
    % is not, then try to load the web_404 page. If that
    % is not available, then default to the 'file_not_found_page' module.
    case code:ensure_loaded(Module) of
        {module, Module} -> {Module, PathInfo};
        _ -> 
            case code:ensure_loaded(web_404) of
                {module, web_404} -> {web_404, Path};
                _ -> {file_not_found_page, Path}
            end
    end.
