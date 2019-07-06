-module(n2o_ring).
-copyright('Copyright (c) 2016 Carlos Galdino').
-license('MIT').
-export([add/2,contains/2,lookup/2,members/1,new/1,new/2,remove/2,size/1,send/2]).
-define(HASH, sha256).

-type num_vnodes() :: pos_integer().
-type node_entry() :: term().
-type key() :: term().
-type position() :: binary().
-type positions() :: [{position(), node_entry()}].
-type nodes() :: [node_entry()].
-type inner_ring() :: gb_trees:tree(position(), node_entry()).
-type ring_index() :: {num_vnodes(), inner_ring()}.
-type ring() :: tcp | ws | mqtt.

-spec ring(ring()) -> any().
-spec add(node_entry(), ring()) -> ring_index().
-spec contains(node_entry(), ring()) -> boolean().
-spec lookup(key(), ring()) -> node_entry() | {error, empty_ring}.
-spec position_node(num_vnodes(), node_entry()) -> positions().
-spec members(ring()) -> nodes().
-spec new(nodes()) -> ring_index().
-spec new(num_vnodes(), nodes()) -> ring_index().
-spec remove(node_entry(), ring()) -> ring_index().

tab2ring(ws)       -> ws_ring;
tab2ring(mqtt)     -> mqtt_ring;
tab2ring(tcp)      -> tcp_ring.
ring(Tab)          -> application:get_env(n2o,tab2ring(Tab),n2o_ring:new(1,[1])).
lookup(Tab,Term)   -> lookup_index(Term, ring(Tab)).
contains(Tab,Term) -> contains_index(Term, ring(Tab)).
add(Tab,Node)      -> add_index(Node, ring(Tab)).
remove(Tab,Node)   -> remove_index(Node, ring(Tab)).
size(Tab)          -> size_index(Tab).
send(Tab,Msg)      -> n2o_pi:send(Tab,lookup_index(Msg, ring(Tab)),Msg).
members(Tab)       -> members_index(ring(Tab)).

add_index(Node, {NumVNodes, InnerRing}) ->
    NewInnerRing = build_ring(position_node(NumVNodes, Node), InnerRing),
    {NumVNodes, NewInnerRing}.

contains_index(Node, {_NumVNodes, InnerRing}) ->
    case gb_trees:lookup(chash(Node), InnerRing) of
        none -> false;
        {value, _} -> true
    end.

lookup_index(Key, {_NumVNodes, InnerRing}) ->
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

new(NumVNodes, Nodes) ->
    Ring = build_ring(lists:flatten([position_node(NumVNodes, Node) || Node <- Nodes])),
    {NumVNodes, Ring}.

remove_index(Node, {NumVNodes, InnerRing}) ->
    Positions = position_node(NumVNodes, Node),
    NewInnerRing = lists:foldl(fun({Pos, _}, Tree) -> gb_trees:delete_any(Pos, Tree) end, InnerRing, Positions),
    {NumVNodes, NewInnerRing}.

size_index({_NumVNodes, InnerRing}) -> gb_trees:size(InnerRing).
build_ring(Nodes) -> gb_trees:from_orddict(lists:keysort(1, Nodes)).
build_ring(Nodes, Ring) -> lists:foldl(fun({Pos, Node}, Tree) -> gb_trees:insert(Pos, Node, Tree) end, Ring, Nodes).
members_index({_NumVNodes, InnerRing}) -> lists:usort(gb_trees:values(InnerRing)).
new(Nodes) -> new(1, Nodes).
chash(X) -> crypto:hash(?HASH, term_to_binary(X)).
chash(X, Y) -> XBin = term_to_binary(X), YBin = term_to_binary(Y), crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).
position_node(Node) -> {chash(Node), Node}.
position_node(1, Node) -> [position_node(Node)];
position_node(NumVNodes, Node) ->
    Replicas = [{chash(Node, Idx), Node} || Idx <- lists:seq(1, NumVNodes - 1)],
    [position_node(Node) | Replicas].
