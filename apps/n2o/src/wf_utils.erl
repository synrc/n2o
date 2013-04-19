% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_utils).
-include_lib ("wf.hrl").
-export ([
    f/1, f/2,
    guid/0, short_guid/0,
    path_search/3,
    replace/3,
    coalesce/1,
    is_process_alive/1,
    debug/0, break/0,
    get_elementbase/1, get_actionbase/1, get_validatorbase/1, replace_with_base/2,
    indexof/2,
    replace_field/4,
    get_field/3
]).

-define(COPY_TO_BASERECORD(Name, Size, Record),
    list_to_tuple([Name | lists:sublist(tuple_to_list(Record), 2, Size-1)])).

%%% FORMAT %%%

f(S) -> f(S, []).
f(S, Args) -> lists:flatten(io_lib:format(S, Args)).


%%% IDS %%%

% guid/0 - Return a guid like object.
guid() ->
    MD5 = erlang:md5(term_to_binary({node(), now(), make_ref()})),
    MD5List = lists:nthtail(8, binary_to_list(MD5)),
    F = fun(N) -> wf:f("~2.16.0B", [N]) end,
    L = [F(N) || N <- MD5List],
    lists:flatten(L).

% short_guid/0 - Return a shorter guid like object.
short_guid() ->
    MD5 = erlang:md5(term_to_binary({node(), now(), make_ref()})),
    MD5List = lists:nthtail(14, binary_to_list(MD5)),
    F = fun(N) -> wf:f("~2.16.0B", [N]) end,
    L = [F(N) || N <- MD5List],
    lists:flatten(L).

is_process_alive(Pid) ->
    case is_pid(Pid) of
        true -> 
            % If node(Pid) is down, rpc:call returns something other than
            % true or false.
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                true -> true;
                _ -> false
            end;
        _ -> false
    end.


%%% XPATH STYLE QUERY LOOKUPS %%%

% path_search/2 - search for part of a specified path within a list of paths.
% Partial = [atom3, atom2, atom1]
% Paths=[[atom3, atom2, atom1], [atom5, atom4...]]
% Conducts 
path_search(Partial, N, Paths) -> path_search(Partial, N, Paths, 1).
path_search(_, _, [], _) -> [];
path_search([], _, Paths, _) -> Paths;
path_search(['*'], _, Paths, _) -> Paths;
path_search(_, _, _, 10) -> [];
path_search(['*'|T], N, Paths, Pos) ->
    % We have a wildcard so everything matches. 
    % Split into two new searches.
    path_search(['*'|T], N, Paths, Pos + 1) ++ path_search(T, N, Paths, Pos + 1);

path_search([H|T], N, Paths, Pos) ->
    % Return all Paths for which H matches the Nth element.
    F = fun(Tuple) -> 
        Path = erlang:element(N, Tuple),
        (Pos =< length(Path)) andalso (H == lists:nth(Pos, Path)) 
    end,
    Paths1 = lists:filter(F, Paths),
    path_search(T, N, Paths1, Pos + 1).

%%% STRING REPLACE %%%

replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
    Length = length(S1),
    case string:substr(String, 1, Length) of 
        S1 -> 
            S2 ++ replace(string:substr(String, Length + 1), S1, S2);
        _ -> 
            [hd(String)|replace(tl(String), S1, S2)]
    end.


%%% COALESCE %%%

coalesce([]) -> undefined;
coalesce([H]) -> H;
coalesce([undefined|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.

%%% BASE RECORDS %%%

get_actionbase(Term) -> ?COPY_TO_BASERECORD(actionbase, size(#actionbase{}), Term).
get_elementbase(Term) -> ?COPY_TO_BASERECORD(elementbase, size(#elementbase{}), Term).
get_validatorbase(Term) -> ?COPY_TO_BASERECORD(validatorbase, size(#validatorbase{}), Term).

replace_with_base(Base, Record) -> 
    RecordType = element(1, Record),
    BaseMiddle = tl(tuple_to_list(Base)),
    Start = size(Base) + 1,
    Len = size(Record) - Start + 1,
    RecordEnd = lists:sublist(tuple_to_list(Record), Start, Len),
    list_to_tuple([RecordType] ++ BaseMiddle ++ RecordEnd).

%%% DEBUG %%%

debug() ->
    % Get all web and wf modules.
    F = fun(X) ->
        {value, {source, Path}} = lists:keysearch(source, 1, X:module_info(compile)), Path
    end,

    L =  [list_to_binary(atom_to_list(X)) || X <- erlang:loaded()],
    ModulePaths = 
    [F(wf)] ++
    [F(list_to_atom(binary_to_list(X))) || <<"web_", _/binary>>=X <- L] ++
    [F(list_to_atom(binary_to_list(X))) || <<"wf_", _/binary>>=X <- L],

    i:im(),
    i:ii(ModulePaths),

    i:iaa([break]),
    i:ib(?MODULE, break, 0).

break() -> ok.

indexof(Key, Fields) -> indexof(Key, Fields, 2).
indexof(_Key, [], _N) -> undefined;
indexof(Key, [Key|_T], N) -> N;
indexof(Key, [_|T], N) -> indexof(Key, T, N + 1).

replace_field(Key, Value, Fields, Rec) ->
	N = indexof(Key, Fields),
	setelement(N, Rec, Value).

get_field(Key, Fields, Rec) ->
	case indexof(Key, Fields) of
		undefined -> undefined;
		N -> element(N, Rec)
	end.
