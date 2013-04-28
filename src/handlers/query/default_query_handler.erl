% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

% Though this is defined as a handler, it is unlikely
% that anyone would want to override the default behaviour. 
% It is defined as a handler simply because it fit well 
% into the existing handler pattern.

-module (default_query_handler).
-behaviour (query_handler).
-include_lib ("wf.hrl").
-export ([
    init/2, 
    finish/2,
    get_value/3,
    get_values/3,
    get_params/2
]).

init(_Config, _State) -> 
    % Get query params and post params
    % from the request bridge...
    RequestBridge = wf_context:request_bridge(),
    QueryParams = RequestBridge:query_params(),
    PostParams = RequestBridge:post_params(),

    % Load into state...
    Params = QueryParams ++ PostParams,

    % Pre-normalize the parameters.
    Params1 = [{normalize_path(Path), Value} || {Path, Value} <- Params, Path /= undefined, Path /= []],
    {ok, Params1}.

finish(_Config, _State) -> 
    % Clear out the state.
    {ok, []}.

%% Given a path, return the value that matches the path.
get_value(Path, Config, State) ->
    case get_values(Path, Config, State) of
        [] -> undefined;
        [One] -> One;
        _Many -> throw({?MODULE, too_many_matches, Path})
    end.

get_values(Path, _Config, State) ->
    Params = State,
    Path1 = normalize_path(Path),
    refine_params(Path1, Params).    

get_params(_Config, State) ->
    Params = State,
    F = fun({KeyPartsReversed, Value}) ->
        KeyParts = lists:reverse(KeyPartsReversed),
        Key = string:join(KeyParts, "."),
        { Key, Value }
    end,
    lists:map(F, Params).

%% Next, narrow down the parameters by keeping only the parameters
%% that contain the next element found in path, while shrinking the 
%% parameter paths at the same time.
%% For example, if:
%% 	Path   = [a, b, c] 
%% 	Params = [{[x, a, y, b, c], _}] 
%% Then after the first round of refine_params/2 we would have:
%%   Path   = [b, c]
%%   Params = [y, b, c]
refine_params([], Params) -> 
    [V || {_, V} <- Params];
refine_params([H|T], Params) ->
    F = fun({Path, Value}, Acc) ->
        case split_on(H, Path) of
            {ok, RemainingPath} -> [{RemainingPath, Value}|Acc];
            false -> Acc
        end
    end,
    Params1 = lists:foldl(F, [], Params),
    refine_params(T, lists:reverse(Params1)).

split_on(_,  []) -> false;
split_on(El, [El|T]) -> {ok, T};
split_on(El, [_|T]) -> split_on(El, T).

normalize_path(Path) when is_atom(Path) ->
    normalize_path(atom_to_list(Path));

normalize_path(Path) when ?IS_STRING(Path) ->
    Tokens = string:tokens(Path, "."),
    Tokens1 = [strip_wfid(X) || X <- Tokens],
    lists:reverse(Tokens1).

%% Most tokens will start with "wfid_". Strip this out.
strip_wfid(Path) ->
    case Path of 
        "wfid_" ++ S -> S;
        S -> S
    end.


