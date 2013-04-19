% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_bind).
-include_lib ("wf.hrl").
-compile(export_all).

% Transform function is of the form:
% Function(DataRow, Acc) -> {NewDataRow, NewAcc, ExtraBindings}.
% <ExtraBindings> -> {field@attr}, Value}.

reflect() -> record_info(fields, bind).

render_element(Record) -> 
    % Get attributes.
    Data = Record#bind.data,
    Map = Record#bind.map,
    Transform = case Record#bind.transform of 
        undefined -> fun(A, B) -> {A, B, []} end;
        Other -> Other
    end,
    AccInit = Record#bind.acc,
    Body = Record#bind.body,

    % Bind the data to the body template...
    case length(Data) > 0 of
        true ->	
            Body1 = bind(Body, Data, Map, Transform, AccInit),
            % Render the new body to html...
            render_rows(Body1, 1);

        _ ->
            Record#bind.empty_body
    end.

render_rows([], _) -> [];
render_rows([H|T], N) ->
    ID = "row" ++ wf:to_list(N),
    Placeholder = #placeholder { id=ID, body=H },
    [Placeholder|render_rows(T, N + 1)].


%% bind/5 - 
%% Given a Body of elements, a list of Data, a Map
%% applying the data to elements, and a Transform function with Acc,
%% bind the data to the elements.
bind(_, [], _, _, _) -> [];
bind(Body, [DataRow|Data], Map, Transform, Acc) ->
    % Run the transform function...
    {DataRow1, Acc1, ExtraBindings} = Transform(DataRow, Acc),

    % Map the data into the body...
    RawBindings = extract_bindings(Map, DataRow1),
    Bindings = normalize_bindings([[RawBindings] ++ [ExtraBindings]]),
    Body1 = apply_bindings(Bindings, Body),

    % Iterate.
    [Body1|bind(Body, Data, Map, Transform, Acc1)].


%%% APPLY_BINDING %%%

%% apply_bindings/2 - Given a list of terms and a list
%% of replacements, update the terms with values from replacements.
apply_bindings(Bindings, Term) when is_list(Term) ->
    case Term == [] orelse ?IS_STRING(Term) of
        true -> Term;
        false -> [apply_bindings(Bindings, X) || X <- Term]
    end;

apply_bindings(Bindings, Term) when is_tuple(Term) ->
    Base = wf_utils:get_elementbase(Term),
    TypeModule = Base#elementbase.module,
    Fields = TypeModule:reflect(),
    ID = wf:to_list(Base#elementbase.id),

    % Do replacements on this term...
    F1 = fun(Binding, Rec) ->
        {{RepID, RepAttr}, Value} = Binding,
        case RepID == ID of
            true -> replace_field(RepAttr, Value, Fields, Rec);
            false -> Rec
        end
    end,
    Term1 = lists:foldl(F1, Term, Bindings),

    % Do replacements on children (in 'body', 'rows', or 'cells' fields)...	
    F2 = fun(ChildField, Rec) ->
        case get_field(ChildField, Fields, Term1) of
            undefined -> Rec;
            Children -> 
                Children1 = apply_bindings(Bindings, Children),
                replace_field(ChildField, Children1, Fields, Rec)
        end
    end,
    lists:foldl(F2, Term1, [body, empty_body, rows, cells]);

apply_bindings(_, Term) -> Term.


get_field(Key, Fields, Rec) -> 
    case indexof(Key, Fields) of
        undefined -> undefined;
        N -> element(N, Rec)
    end.

replace_field(Key, Value, Fields, Rec) ->
    N = indexof(Key, Fields),
    setelement(N, Rec, Value).	

indexof(Key, Fields) -> indexof(Key, Fields, 2).
indexof(_Key, [], _) -> undefined;
indexof(Key, [Key|_T], N) -> N;
indexof(Key, [_|T], N) -> indexof(Key, T, N + 1).

%%% NORMALIZE_BINDING %%%
normalize_bindings(Bindings) ->
    Bindings1 = lists:flatten([Bindings]),
    Bindings2 = [{get_replacement_key_parts(Key), Value} || {Key, Value} <- Bindings1],
    [{Key, Value} || {Key, Value} <- Bindings2, Key /= ignore].

get_replacement_key_parts(Key) ->
    case string:tokens(wf:to_list(Key), "@") of
        [ID, Attr] -> {ID, wf:to_atom(Attr)};
        _ -> ignore
    end.



%%% EXTRACT_BINDING %%%

%% extract_bindings/2 - Return a list of tuples
%% of the form {element1.element2@attr, Value}.

% Map and Data are lists. 
% Treat either as keyvalue pairs, or just walk through the list.
extract_bindings(Map, Data) when is_list(Map), is_list(Data) ->
    case is_keyvalue(Map) andalso is_keyvalue(Data) of
        true -> extract_bindings_from_keyvalue(Map, Data);
        false -> extract_bindings_from_list(Map, Data)
    end;

% Map and Data are tuples.
% Convert the tuples to lists and walk through them.
extract_bindings(Map, Data) when is_tuple(Map), is_tuple(Data) ->
    extract_bindings_from_list(tuple_to_list(Map), tuple_to_list(Data));

% Map and Data are atoms.
% Treat it as a successful map.
extract_bindings(Map, Data) when is_atom(Map) -> 
    {Map, Data};

% If we made it here, something is wrong, throw an exception.
extract_bindings(Map, Data) ->
    erlang:throw({invalid_binding, Map, Data}).

% Return true if List contains only {Key, Value} pairs.
is_keyvalue(List) ->
    F = fun(X) ->
        is_tuple(X) andalso is_atom(element(1, X))
    end,
    lists:all(F, List).

% Walk through the Map to get the element@attr terms.
% Then, look up the corresponding value in the data.
extract_bindings_from_keyvalue([], _) -> [];
extract_bindings_from_keyvalue([HMap|TMap], Data) ->
    {Key, ElementAttr} = HMap,
    case proplists:get_value(Key, Data) of
        undefined -> extract_bindings_from_keyvalue(TMap, Data);
        Value -> 
            [
                extract_bindings(ElementAttr, Value) |
                extract_bindings_from_keyvalue(TMap, Data)
            ]
    end.

% Call extract_bindings on each pair of Data/Map.
extract_bindings_from_list([], []) -> [];
extract_bindings_from_list([HMap|TMap], [HData|TData]) -> [extract_bindings(HMap, HData)|extract_bindings_from_list(TMap, TData)];	
extract_bindings_from_list(Map, Data) -> erlang:throw({leftover_binding, Map, Data}).



