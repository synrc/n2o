-module(wf_context).
-author('Rusty Klophous').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

context() -> get(context).
context(Context) -> put(context, Context).

add_action(Action) ->
    Context = context(),
    Actions = Context#context.queued_actions,
    context(Context#context { queued_actions=lists:flatten([Action|Actions]) }).

actions() -> Actions = (context())#context.queued_actions, lists:reverse(Actions).
actions(Actions) -> context((context())#context { queued_actions = lists:reverse(Actions) }).
clear_actions() -> context((context())#context { queued_actions=[] }).
page_context() -> (context())#context.page_context.
page_context(PageContext) -> context((context())#context { page_context = PageContext }).
series_id() -> (page_context())#page_context.series_id.
series_id(SeriesID) -> page_context((page_context())#page_context { series_id = SeriesID }).
page_module() -> (page_context())#page_context.module.
page_module(Module) -> page_context((page_context())#page_context { module = Module }).
path_info() -> (page_context())#page_context.path_info.
path_info(PathInfo) -> page_context((page_context())#page_context { path_info = PathInfo }).
event_context() -> (context())#context.event_context.
event_context(EventContext) -> context((context())#context { event_context = EventContext }).
event_module() -> (event_context())#event_context.module.
event_module(Module) -> event_context((event_context())#event_context { module = Module }).
event_tag() -> (event_context())#event_context.tag.
event_tag(Tag) -> event_context((event_context())#event_context { tag = Tag }).
event_validation_group() -> (event_context())#event_context.validation_group.
event_validation_group(ValidationGroup) -> event_context((event_context())#event_context { validation_group = ValidationGroup }).
handlers() -> (context())#context.handler_list.
handlers(Handlers) -> context((context())#context { handler_list = Handlers }).
request_body() -> (request_bridge()):request_body().
status_code() -> (request_bridge()):status_code().
status_code(StatusCode) -> response_bridge((response_bridge()):status_code(StatusCode)).
content_type(ContentType) -> response_bridge((response_bridge()):header("Content-Type", ContentType)).
headers() -> (request_bridge()):headers().
header(Header) -> (request_bridge()):header(Header).
header(Header, Value) -> response_bridge((response_bridge()):header(Header, Value)).
cookies() -> (request_bridge()):cookies().
cookie(Cookie) when is_atom(Cookie) -> cookie(atom_to_list(Cookie));
cookie(Cookie) -> (request_bridge()):cookie(Cookie).
cookie_default(Cookie,DefaultValue) ->
    case cookie(Cookie) of
        undefined -> DefaultValue;
        Value -> Value
    end.

cookie(Cookie, Value) -> response_bridge((response_bridge()):cookie(Cookie, Value)).
cookie(Cookie, Value, Path, MinutesToLive) -> response_bridge((response_bridge()):cookie(Cookie, Value, Path, MinutesToLive)).
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

init_context(RequestBridge, ResponseBridge) ->
    Context = #context {
        request_bridge = RequestBridge,
        response_bridge = ResponseBridge,
        page_context = #page_context { series_id = wf:temp_id() },
        event_context = #event_context {},
        handler_list = [
%            make_handler(config_handler, default_config_handler), 
%            make_handler(log_handler, default_log_handler),
%            make_handler(process_registry_handler, gproc_registry_handler),
%            make_handler(cache_handler, default_cache_handler), 
            make_handler(query_handler, default_query_handler),
            make_handler(session_handler, n2o_session_handler), 
%            make_handler(state_handler, default_state_handler), 
%            make_handler(identity_handler, default_identity_handler), 
%            make_handler(role_handler, default_role_handler), 
            make_handler(route_handler, dynamic_route_handler)
 %            make_handler(security_handler, default_security_handler)
        ]
    },
    context(Context),
    Context.

make_handler(Name, Module) -> 
    #handler_context { 
        name=Name,
        module=Module,
        state=[]
    }.

