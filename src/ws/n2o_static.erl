-module(n2o_static).
-export([init/2,endpoints/2, index/0, websocket_info/2, websocket_init/1, websocket_handle/2,
         malformed_request/2, forbidden/2, content_types_provided/2, ranges_provided/2,
         resource_exists/2, last_modified/2, generate_etag/2, get_file/2]).

malformed_request(R,S)      -> cowboy_static:malformed_request(R,S).
forbidden(R,S)              -> cowboy_static:forbidden(R,S).
content_types_provided(R,S) -> cowboy_static:content_types_provided(R,S).
ranges_provided(R,S)        -> cowboy_static:ranges_provided(R,S).
resource_exists(R,S)        -> cowboy_static:resource_exists(R,S).
last_modified(R,S)          -> cowboy_static:last_modified(R,S).
generate_etag(R,S)          -> cowboy_static:generate_etag(R,S).
get_file(R,S)               -> cowboy_static:get_file(R,S).
websocket_init(S)           -> n2o_cowboy:websocket_init(S).
websocket_handle(D,S)       -> n2o_cowboy:websocket_handle(D,S).
websocket_info(D,S)         -> n2o_cowboy:websocket_info(D,S).

index() -> [n2o_cowboy:fix1(code:priv_dir(?MODULE)), "/static/index.html"].

init(#{headers := #{<<"upgrade">> := <<"websocket">>}} = Req, _) -> {cowboy_websocket, Req, Req};
init(Req, _) ->
    Index = filename:absname(iolist_to_binary(index())),
    {ok, Info} = file:read_file_info(Index, [{time, universal}]),
    {cowboy_rest, Req, {Index, {direct, Info}, []}}.

endpoints(App,Module) ->
    S = {dir, n2o_cowboy:fix1(code:priv_dir(App))++"/static", []},
    cowboy_router:compile([{'_', [{"/", Module, []},
        {"/app/[...]", cowboy_static, S},
        {"/[...]", cowboy_static, S}]}]).
