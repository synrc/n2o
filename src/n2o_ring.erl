-module(n2o_ring).

-description('N2O Ring').

-export([add/3,
         contains/3,
         lookup/3,
         members/1,
         new/1,
         new/2,
         remove/3,
         size/1,
         send/2,
         send/3]).

-export([ring/2, tab2srv/1, tab2ring/1]).

-define(HASH, sha).

tab2ring(ws) -> ws_ring;
tab2ring(mqtt) -> mqtt_ring;
tab2ring(tcp) -> tcp_ring.

tab2srv(ws) -> ws_services;
tab2srv(mqtt) -> mqtt_services;
tab2srv(tcp) -> tcp_services.

ring({App, _}, Tab) -> ring(App, Tab);
ring(App, Tab) ->
    case application:get_env(App, tab2ring(Tab)) of
        {ok, R} -> R;
        undefined -> n2o_ring:new(1, [1])
    end.

lookup(Tab, App, Term) ->
    lookup_index(Term, ring(App, Tab)).

contains(Tab, App, Term) ->
    contains_index(Term, ring(App, Tab)).

add(Tab, App, Node) -> add_index(Node, ring(App, Tab)).

remove(Tab, App, Node) ->
    remove_index(Node, ring(App, Tab)).

size(Tab) -> size_index(Tab).

send(Tab, Msg) ->
    n2o_pi:send(Tab,
                lookup_index(Msg, ring(n2o, Tab)),
                Msg).

send(Tab, App, Msg) ->
    Name = lookup_index(Msg, ring(App, Tab)),
    n2o_pi:send(n2o_pi:pid(Tab, Name), {ring, App, Msg}).

members(Tab) ->
    [Z
     || {Z}
            <- lists:flatten([lists:map(fun (X) -> {X} end,
                                        members_index(ring(App, Tab)))
                              || App <- application:get_env(n2o, tab2srv(Tab), [])])].

add_index(Node, {NumVNodes, InnerRing}) ->
    NewInnerRing = build_ring(position_node(NumVNodes,
                                            Node),
                              InnerRing),
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
    Ring =
        build_ring(lists:flatten([position_node(NumVNodes, Node)
                                  || Node <- Nodes])),
    {NumVNodes, Ring}.

remove_index(Node, {NumVNodes, InnerRing}) ->
    Positions = position_node(NumVNodes, Node),
    NewInnerRing = lists:foldl(fun ({Pos, _}, Tree) ->
                                       gb_trees:delete_any(Pos, Tree)
                               end,
                               InnerRing,
                               Positions),
    {NumVNodes, NewInnerRing}.

size_index({_NumVNodes, InnerRing}) ->
    gb_trees:size(InnerRing).

build_ring(Nodes) ->
    gb_trees:from_orddict(lists:keysort(1, Nodes)).

build_ring(Nodes, Ring) ->
    lists:foldl(fun ({Pos, Node}, Tree) ->
                        gb_trees:insert(Pos, Node, Tree)
                end,
                Ring,
                Nodes).

members_index({_NumVNodes, InnerRing}) ->
    lists:usort(gb_trees:values(InnerRing)).

new(Nodes) -> new(1, Nodes).

chash(X) -> crypto:hash(?HASH, term_to_binary(X)).

chash(X, Y) ->
    XBin = term_to_binary(X),
    YBin = term_to_binary(Y),
    crypto:hash(?HASH, <<XBin/binary, YBin/binary>>).

position_node(Node) -> {chash(Node), Node}.

position_node(1, Node) -> [position_node(Node)];
position_node(NumVNodes, Node) ->
    Replicas = [{chash(Node, Idx), Node}
                || Idx <- lists:seq(1, NumVNodes - 1)],
    [position_node(Node) | Replicas].
