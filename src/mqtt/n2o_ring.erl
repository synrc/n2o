-module(n2o_ring).
-description('N2O Ring').
-include("n2o.hrl").
-export([ring/0,init/1,send/1,lookup/1,add/1,delete/1]).
-record(state, { ring, nodes }).

send(Msg) ->
    {ring,VNode} = n2o_ring:lookup(Msg),
    n2o_async:send(ring,VNode,Msg).

node_shares() ->
    Partitions = partitions(),
    NodePartitions = fun(Node) ->
            lists:foldl(fun
                    ({RN, From, To}, Acc) when RN == Node -> Acc + (To - From);
                    (_, Acc) -> Acc
                end, 0, Partitions)
    end,
    lists:flatten([io_lib:format("\t~p weight ~p share ~.2f%~n",
                [Node, Weight, Share])
            || {Node, _, Weight} <- get_config(),
            Share <- [100 * NodePartitions(Node) / 65536]]).

partitions_if_node_added(Node) ->
    Nodes = get_config(),
    {ok, S} = init([Node | Nodes]),
    partitions_from_ring(S#state.ring).

%% gen_server callbacks

add(Nodes) ->
    OldNodes = get_config(),
    case [ N || {N, _, _} <- Nodes, lists:keymember(N, 1, OldNodes) ] of
        []       -> init(OldNodes ++ Nodes);
        Overlaps -> {error, {already_there, Overlaps}}
    end.

delete(Nodes) ->
    OldNodes = get_config(),
    case [ N || N <- Nodes, not lists:keymember(N, 1, OldNodes) ] of
        [] ->
            init([Node || {N, _, _} = Node <- OldNodes, not lists:member(N, Nodes)]),
            ok;
        NotThere -> {error, {unknown_nodes, NotThere}}
    end.

get_config() -> application:get_env(n2o,nodes,[]).
ring()       -> application:get_env(n2o,ring,[]).
ring_list()  -> array:to_list(ring()).
lookup(Key)  -> lookup_index(index1(Key)).
partitions() -> partitions_from_ring(ring()).
nodes()      -> [{Name, Opaque} || {Name, Opaque, _} <- get_config() ].

lookup_index(KeyIndex) ->
    Ring = application:get_env(n2o,ring,[]),
    true = (KeyIndex >= 0) andalso (KeyIndex < 65536),
    case bsearch(Ring, KeyIndex) of
        empty -> [];
        PartIdx -> {ring,PartIdx+1}
    end.

set_opaque({Name, Opaque}) ->
    NewNodes = lists:map(fun
            ({N, _OldOpaque, Weight}) when N == Name -> {N, Opaque, Weight};
            (V) -> V
        end, get_config()),
    NewRing = array:from_list(lists:map(fun({Hash, Data}) ->
                    {Hash, lists:map(fun
                                ({N, _OldOpaque}) when N == Name -> {N, Opaque};
                                (V) -> V
                            end, Data)}
            end, ring_list())),
    application:set_env(n2o,nodes,NewNodes),
    application:set_env(n2o,ring,NewRing),
    {ok, #state{ring=NewRing,nodes=NewNodes}}.

init(Peers) ->
    _ = lists:sum([ C||{_,_,C} <- Peers]),
    RawRing = lists:keysort(1,
        [ begin
            {H, {Node, Opaque}} end || {Node, Opaque, Weight} <- Peers,
            N <- lists:seq(1, Weight),
            H <- [index1([atom_to_list(Node), integer_to_list(N)])]
        ]
    ),
    Ring = array:from_list(assemble_ring([], lists:reverse(RawRing), [], length(Peers))),
    n2o:info(?MODULE,"RING: ~p~n", [array:sparse_size(Ring)]),
    application:set_env(n2o,ring,Ring),
    application:set_env(n2o,nodes,Peers),
    {ok, #state{ring=Ring,nodes=Peers}}.

%% Internal functions

assemble_ring(_, [], R, _) -> R;
assemble_ring(H,[{Hash, {NN, _} = N} |T],R,L) ->
    ITN = [N|[E || {N2,_} = E<-H, N2 /= NN]],
    LITN = length(ITN),
    TN = case LITN == L of
        true -> ITN;
        false ->
            {_, RN} = try lists:foldr(
                    fun(_, {L2, Acc}) when L2==L -> throw({L2, Acc});
                        ({_, {N2, _} = E}, {L2, Acc}) ->
                            case lists:keymember(N2, 1, Acc) of
                                true -> {L2, Acc};
                                false -> {L2+1, Acc++[E]}
                            end
                    end, {LITN, ITN}, T)
                catch throw:V -> V end,
            RN
    end,
    assemble_ring(ITN,T,[{Hash,TN}|R],L).

calc_partitions([{Idx, [{Node, _} | _]}], FirstIdx, Acc) ->
    [{Node, 0, FirstIdx}, {Node, Idx, 65536} | Acc];
calc_partitions([{Idx1, [{Node, _} | _]}, {Idx2, _} = E | T], FirstIdx, Acc) ->
    calc_partitions([E|T], FirstIdx, [{Node, Idx1, Idx2} | Acc]).

partitions_from_ring(Ring) ->
    ArrL = array:to_list(Ring),
    [{Idx, _} | _] = ArrL,
    calc_partitions(ArrL, Idx, []).

index1(Key) ->
    <<A,B,_/bytes>> = erlang:md5(term_to_binary(Key)),
    (A bsl 8 + B).

bsearch(Arr, K) ->
    Size =  array:sparse_size(Arr),
    if Size == 0 -> empty; true -> bsearch(Arr, Size, 0, Size - 1, K) end.

bsearch(Arr, Size, LIdx, RIdx, K) ->
    MIdx = LIdx + (RIdx - LIdx + 1) div 2,
    true = (MIdx >= LIdx) andalso (MIdx =< RIdx),
    case key_fits(Arr, Size, MIdx - 1, MIdx, K) of
        {yes, Idx} -> Idx;
        {no, lt} -> bsearch(Arr, Size, LIdx, MIdx, K);
        {no, gt} ->
            if
                MIdx == (Size - 1) -> Size - 1;
                true -> bsearch(Arr, Size, MIdx, RIdx, K)
            end
    end.

key_fits(_Arr, 1, -1, 0, _K) ->
    {yes, 0};

key_fits(Arr, Size, -1, 0, K) ->
    {Hash0, _} = array:get(0, Arr),
    {HashS, _} = array:get(Size - 1, Arr),
    true = K < HashS,
    if
        K < Hash0 -> {yes, Size - 1};
        true -> {no, gt}
    end;

key_fits(Arr, Size, L, R, K) ->
    {LHash, _} = array:get(L, Arr),
    {RHash, _} = array:get(R, Arr),
    if
        K < LHash -> if L == 0 -> {yes, Size - 1}; true -> {no, lt} end;
        (K >= LHash) andalso (K < RHash) -> {yes, L};
        K >= RHash -> {no, gt}
    end.
