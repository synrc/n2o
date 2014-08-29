-module(wf).
-author('Rusty Klophaus').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o/include/api.hrl").
-compile (export_all).

% Here is Nitrogen Web Framework compatible API
% Please read major changes made to N2O and
% how to port existing Nitrogen sites at http://synrc.com/framework/web/

% Update DOM wf:update

update(Target, Elements) ->
    wf:wire(#jq{target=Target,property=outerHTML,right=Elements,format="'~s'"}).

insert_top(Target, Elements) ->
    Pid = self(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,wf_context:actions()} end),
    {Render,Actions} = receive A -> A end,
    wf:wire(wf:f(
        "document.querySelector('#~s').insertBefore("
        "(function(){var div = document.createElement('div');"
        "div.innerHTML = '~s'; return div.firstChild; })(),"
        "document.querySelector('#~s').firstChild);",
        [Target,Render,Target])),
    wf:wire(wf:render(Actions)).

insert_bottom(Target, Elements) ->
    Pid = self(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,wf_context:actions()} end),
    {Render,Actions} = receive A -> A end,
    wf:wire(wf:f(
        "document.querySelector('#~s').appendChild("
        "(function(){var div = document.createElement('div');"
        "div.innerHTML = '~s'; return div.firstChild; })());",
        [Target,Render])),
    wf:wire(wf:render(Actions)).

insert_adjacent(Command,Target, Elements) ->
    Pid = self(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,wf_context:actions()} end),
    {Render,Actions} = receive A -> A end,
    wf:wire(wf:f("document.querySelector('#~s').insertAdjacentHTML('~s', '~s');",
        [Target,Command,Render])),
    wf:wire(wf:render(Actions)).

insert_before(Target, Elements) -> insert_adjacent(beforebegin,Target, Elements).
insert_after(Target, Elements) -> insert_adjacent(afterend,Target, Elements).

remove(Target) ->
    wf:wire(wf:f(
        "document.querySelector('#~s').parentNode.removeChild("
        "document.querySelector('#~s'));",[Target,Target])).

% Wire JavaScript wf:wire

wire(Actions) -> action_wire:wire(Actions).

% Spawn async processes wf:async, wf:flush

comet(Function) -> async(Function). % legacy name
async(Function) -> action_async:async(Function).
async(Name,Function) -> action_async:async(Name,Function).
flush(Key) -> action_async:flush(Key).

% Redirect and purge connection wf:redirect

redirect(Url) ->
    wf:wire(#jq{target=window,property=location,args=simple,right=Url}).

% Message Bus communications wf:reg wf:send

-ifndef(REGISTRATOR).
-define(REGISTRATOR, n2o_mq).
-endif.

send(Pool, Message) -> ?REGISTRATOR:send(Pool,Message).
reg(Pool) -> ?REGISTRATOR:reg(Pool).
reg(Pool,Value) -> ?REGISTRATOR:reg(Pool,Value).
unreg(Pool) -> ?REGISTRATOR:unreg(Pool).

% Pickling wf:pickle

-ifndef(PICKLER).
-define(PICKLER, (wf:config(n2o,pickler,n2o_pickle))).
-endif.

pickle(Data) -> ?PICKLER:pickle(Data).
depickle(SerializedData) -> ?PICKLER:depickle(SerializedData).
depickle(SerializedData, TTLSeconds) -> ?PICKLER:depickle(SerializedData, TTLSeconds).

% Error handler wf:error_page

-ifndef(ERRORING).
-define(ERRORING, (wf:config(n2o,erroring,n2o_error))).
-endif.

stack(Error, Reason)    -> ?ERRORING:stack(Error, Reason).
error_page(Class,Error) -> ?ERRORING:error_page(Class, Error).

% Session handling wf:session wf:user wf:role

-ifndef(SESSION).
-define(SESSION, (wf:config(n2o,session,n2o_session))).
-endif.

session(Key) -> ?SESSION:get_value(Key,undefined).
session(Key, Value) -> ?SESSION:set_value(Key, Value).
session_default(Key, DefaultValue) -> ?SESSION:get_value(Key, DefaultValue).
clear_session() -> ?SESSION:clear().
session_id() -> ?SESSION:session_id().
user() -> wf:session(<<"user">>).
user(User) -> wf:session(<<"user">>,User).
clear_user() -> wf:session(<<"user">>,undefined).
logout() -> clear_user(), clear_session().
cache(Key, Value) -> ets:insert(caching,{Key,Value}), Value.
cache(Key) ->
    Res = ets:lookup(caching,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined; {_,X} -> X end.

% Context Variables and URL Query Strings wf:q and wf:qs

q(Key) -> Val = get(Key), case Val of undefined -> qs(Key); A -> A end.
qs(Key) -> proplists:get_value(Key,?CTX#context.params).

% Cookies

cookies() -> wf_context:cookies().
cookie(Name) -> lists:keyfind(Name,1,cookies()).
cookie(Name,Value) -> cookie(Name,Value,"/", 24 * 60 * 60).
cookie(Name,Value,Path,TTL) -> wf_context:add_cookie(Name,Value,Path,TTL).

% Bridge Information

-ifndef(BRIDGE).
-define(BRIDGE, (wf:config(n2o,bridge,n2o_cowboy))).
-endif.

cookie_req(Cookie,Req) -> ?BRIDGE:cookie(Cookie, Req).
cookie_req(Name, Value, Path, TTL, Req) -> ?BRIDGE:cookie(Name, Value, Path, TTL, Req).
params(Req) -> ?BRIDGE:params(Req).
cookies_req(Req) -> ?BRIDGE:cookies(Req).
headers(Req) -> ?BRIDGE:headers(Req).
peer(Req) -> ?BRIDGE:peer(Req).
path(Req) -> ?BRIDGE:path(Req).
request_body(Req) -> ?BRIDGE:request_body(Req).
delete_cookie(Cookie,Req) -> ?BRIDGE:delete_cookie(Cookie,Req).
header(Name,Val,Req) -> ?BRIDGE:header(Name, Val, Req).
response(Html,Req) -> ?BRIDGE:response(Html,Req).
reply(Status,Req) -> ?BRIDGE:reply(Status,Req).

% Logging API

log_modules() -> [].
-define(ALLOWED, (wf:config(n2o,log_modules,wf))).

log(Module, String, Args, Fun) ->
    case lists:member(Module,?ALLOWED:log_modules()) of
         true -> %error_logger:Fun(String, Args);
                   io:format(String ++ "\n\r", Args);
         false -> skip end.

info(Module,String, Args) ->  log(Module,String, Args, info_msg).
info(String, Args) -> log(?MODULE, String, Args, info_msg).
info(String) -> log(?MODULE, String, [], info_msg).

warning(Module,String, Args) -> log(Module, String, Args, warning_msg).
warning(String, Args) -> log(?MODULE, String, Args, warning_msg).
warning(String) -> log(?MODULE,String, [], warning_msg).

error(Module,String, Args) -> log(Module, String, Args, error_msg).
error(String, Args) -> log(?MODULE, String, Args, error_msg).
error(String) -> log(?MODULE, String, [], error_msg).

% Convert and Utils API

f(S) -> _String = wf_utils:f(S).
f(S, Args) -> _String = wf_utils:f(S, Args).
coalesce(L) -> _Value = wf_utils:coalesce(L).
to_list(T) -> _String = wf_convert:to_list(T).
to_atom(T) -> _Atom = wf_convert:to_atom(T).
to_binary(T) -> _Binary = wf_convert:to_binary(T).
to_integer(T) -> _Integer = wf_convert:to_integer(T).
to_string_list(Term) -> _StringList = wf_convert:to_string_list(Term).
clean_lower(S) -> _String = wf_convert:clean_lower(S).
html_encode(S) -> _String = wf_convert:html_encode(S).
html_encode(S, Encode) -> _String = wf_convert:html_encode(S, Encode).
url_encode(S) -> _String = wf_convert:url_encode(S).
url_decode(S) -> _String = wf_convert:url_decode(S).
hex_encode(S) -> _String = wf_convert:hex_encode(S).
hex_decode(S) -> _String = wf_convert:hex_decode(S).
js_escape(String) -> _String = wf_convert:js_escape(String).
join(List,Delimiter) -> _Result = wf_convert:join(List,Delimiter).
json(Json) -> n2o_json:encode(Json).

% These api are not really API

temp_id() -> {_, _, C} = now(), "temp" ++ integer_to_list(C).
append(List, Key, Value) -> case Value of undefined -> List; _A -> [{Key, Value}|List] end.
render(X) -> wf_render:render(X).

config_multiple(Keys) -> [config(Key, "") || Key <- Keys].
config(Key) -> config(n2o, Key, "").
config(App, Key) -> config(App,Key, "").
config(App, Key, Default) -> wf_utils:config(App, Key, Default).

version() -> "1.7.0".
