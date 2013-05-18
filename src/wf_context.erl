-module(wf_context).
-author('Rusty Klophous').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

context() -> get(context).
context(Context) -> put(context, Context).

add_action(Action) ->
    Actions = case get(actions) of undefined -> []; E -> E end,
    put(actions,Actions ++ [Action]).

actions() -> get(actions). %(context())#context.queued_actions, lists:reverse(Actions).
actions(Actions) -> put(actions,Actions). %context((context())#context { queued_actions = lists:reverse(Actions) }).
clear_actions() -> put(actions,[]). %context((context())#context { queued_actions=[] }).
page_module() -> get(page_module).
page_module(Module) -> put(page_module,Module).
path_info() -> get(path_info).
path_info(PathInfo) -> put(path_info,PathInfo).
request_body() -> (request_bridge()):request_body().
status_code() -> (request_bridge()):status_code().
status_code(StatusCode) -> response_bridge((response_bridge()):status_code(StatusCode)).
content_type(ContentType) -> response_bridge((response_bridge()):header("Content-Type", ContentType)).
headers() -> (request_bridge()):headers().
header(Header) -> (request_bridge()):header(Header).
header(Header, Value) -> response_bridge((response_bridge()):header(Header, Value)).
cookies() -> (request_bridge()):cookies().
cookie(Cookie) when is_atom(Cookie) -> cookie(list_to_binary(atom_to_list(Cookie)));
%cookie(Cookie) -> (request_bridge()):cookie(Cookie).
%cookie(Cookie) -> proplists:get_value(Cookie,get(cookies)).
cookie(Cookie) -> {Val,_} = cowboy_req:cookie(Cookie,get(req)), 
%                  error_logger:info_msg("Co: ~p",[Val]),
                  Val. %proplists:get_value(Cookie,get(cookies)).
cookie_default(Cookie,DefaultValue) ->
    case cookie(Cookie) of
        undefined -> DefaultValue;
        Value -> Value
    end.

cookie(Cookie, Value) -> cookie(Cookie,Value,"/",0).
cookie(Name, Value, Path, TTL) -> 
    Options = [{path, Path}, {max_age, TTL}],
%    error_logger:info_msg("Cookie: ~p",[{Name, Value, Options, get(req)}]),
    cowboy_req:set_resp_cookie(Name, Value, Options, get(req)).

%cookie(Cookie, Value) -> response_bridge((response_bridge()):cookie(Cookie, Value)).
%cookie(Cookie, Value, Path, MinutesToLive) -> response_bridge((response_bridge()):cookie(Cookie, Value, Path, MinutesToLive)).
delete_cookie(Cookie) -> cookie(Cookie,"","/",0).
data() -> (context())#context.data.
data(Data) -> context((context())#context { data = Data }).
clear_data() -> context((context())#context { data = [] }).
request_bridge() -> (context())#context.request_bridge.
request_bridge(RequestBridge) -> (context())#context{request_bridge = RequestBridge}.
response_bridge() -> (context())#context.response_bridge.
response_bridge(ResponseBridge) -> context((context())#context{response_bridge = ResponseBridge}).
socket() -> (wf_context:request_bridge()):socket().
peer_ip() -> (request_bridge()):peer_ip().
peer_ip(Proxies) -> peer_ip(Proxies,x_forwarded_for).
peer_ip(Proxies,ForwardedHeader) ->
    ConnIP = peer_ip(),
    case header(ForwardedHeader) of
        undefined -> ConnIP;
        ForwardedIP ->
            case lists:member(ConnIP,Proxies) of
                true -> ForwardedIP;
                false -> ConnIP
            end
    end.

make_handler(Name, Module) -> #handler_context { name=Name, module=Module, state=[] }.
init_context() -> [ make_handler(query_handler, default_query_handler),
                    make_handler(session_handler, n2o_session_handler),
                    make_handler(route_handler, dynamic_route_handler) ].


