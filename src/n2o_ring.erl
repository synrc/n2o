-module(n2o_ring).
-description('N2O Ring').
-export([add/2,contains/2,lookup/2,members/1,new/1,new/2,remove/2,size/1,send/2]).
-export_type([ring/0]).
-define(HASH, sha256).

send(ws,Msg) ->
    R = application:get_env(n2o,ws_ring,n2o_ring:new(1,[1])),
    Node = n2o_ring:lookup(Msg, R),
    n2o_pi:send(ws,Node,Msg).

-type num_vnodes() :: pos_integer().
-type node_entry() :: term().
-type key() :: term().
-type position() :: binary().

-type positions() :: [{position(), node_entry()}].
-type nodes() :: [node_entry()].

-type inner_ring() :: gb_trees:tree(position(), node_entry()).

-opaque ring() :: {num_vnodes(), inner_ring()}.

-spec add(node_entry(), Ring :: ring()) -> ring().
add(Node, {NumVNodes, InnerRing}) ->
    NewInnerRing = build_ring(position_node(NumVNodes, Node), InnerRing),
    {NumVNodes, NewInnerRing}.

%% @doc Returns true if the given node is present in the ring, otherwise false.
-spec contains(node_entry(), Ring :: ring()) -> boolean().
contains(Node, {_NumVNodes, InnerRing}) ->
    case gb_trees:lookup(chash(Node), InnerRing) of
        none -> false;
        {value, _} -> true
    end.

%% @doc Returns the node associated with the given key. Returns an error if the ring is empty.
-spec lookup(key(), Ring :: ring()) -> node_entry() | {error, empty_ring}.
lookup(Key, {_NumVNodes, InnerRing}) ->
    case gb_trees:is_empty(InnerRing) of
        true -> {error, empty_ring};
        false ->
            HKey = chash(Key),
            Iter = gb_trees:iterator_from(HKey, InnerRing),
            case gb_trees:next(Iter) of
                {_, Node, _} -> Node;
                none -> element(2, gb_trees:smallest(InnerRing))
            end
    end.

%% @doc Returns the ordered list of nodes in the ring.
-spec members(Ring :: ring()) -> nodes().
members({_NumVNodes, InnerRing}) ->
    lists:usort(gb_trees:values(InnerRing)).

%% @doc Creates a new ring without virtual nodes.
-spec new(nodes()) -> ring().
new(Nodes) ->
    new(1, Nodes).

%% @doc Creates a new ring with `NumVNodes' of virtual nodes.
-spec new(num_vnodes(), nodes()) -> ring().
new(NumVNodes, Nodes) ->
    Ring = build_ring(lists:flatten([position_node(NumVNodes, Node) || Node <- Nodes])),
    {NumVNodes, Ring}.

%% @doc Removes the given node (and its virtual nodes) from the ring if the node is present in the ring, otherwise does nothing. Returns the new ring.
-spec remove(node_entry(), Ring :: ring()) -> ring().
remove(Node, {NumVNodes, InnerRing}) ->
    Positions = position_node(NumVNodes, Node),
    NewInnerRing = lists:foldl(fun({Pos, _}, Tree) -> gb_trees:delete_any(Pos, Tree) end, InnerRing, Positions),
    {NumVNodes, NewInnerRing}.

%% @doc Returns the number of nodes (including virtual nodes) in the ring.
-spec size(Ring :: ring()) -> non_neg_integer().
size({_NumVNodes, InnerRing}) ->
    gb_trees:size(InnerRing).

-spec build_ring(positions()) -> inner_ring().
build_ring(Nodes) ->
    gb_trees:from_orddict(lists:keysort(1, Nodes)).

-spec build_ring(positions(), inner_ring()) -> inner_ring().
build_ring(Nodes, Ring) ->
    lists:foldl(fun({Pos, Node}, Tree) -> gb_trees:insert(Pos, Node, Tree) end, Ring, Nodes).

chash(X) -> crypto:hash(?HASH, term_to_binary(X)).

chash(X, Y) ->
    XBin = term_to_binary(X),
    YBin = term_to_binary(Y),
    crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).

position_node(Node) ->
    {chash(Node), Node}.

-spec position_node(num_vnodes(), node_entry()) -> positions().
position_node(1, Node) ->
    [position_node(Node)];
position_node(NumVNodes, Node) ->
    Replicas = [{chash(Node, Idx), Node} || Idx <- lists:seq(1, NumVNodes - 1)],
    [position_node(Node) | Replicas].
