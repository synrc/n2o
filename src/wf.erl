-module(wf).
-author('Rusty Klophaus').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o/include/api.hrl").
-compile (export_all).

% Here is Nitrogen Web Framework compatible API
% Please read major changes made to N2O and
% how to port existing Nitrogen sites at https://synrc.com/apps/n2o

% Update DOM wf:update

update(Target, Elements) ->
    wf:wire(#jq{target=Target,property=outerHTML,right=Elements,format="'~s'"}).

insert_top(Tag,Target, Elements) ->
    Pid = self(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {act,{R,wf_context:actions()}} end),
    {Render,Actions} = receive {act,A} -> A end,
    wf:wire(wf:f(
        "qi('~s').insertBefore("
        "(function(){var div = qn('~s'); div.innerHTML = '~s'; return div.firstChild; })(),"
        "qi('~s').firstChild);",
        [Target,Tag,Render,Target])),
    wf:wire(wf:render(Actions)).

insert_bottom(Tag, Target, Elements) ->
    Pid = self(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {act,{R,wf_context:actions()}} end),
    {Render,Actions} = receive {act,A} -> A end,
    wf:wire(wf:f(
        "(function(){ var div = qn('~s'); div.innerHTML = '~s';"
                     "qi('~s').appendChild(div.firstChild); })();",
        [Tag,Render,Target])),
    wf:wire(wf:render(Actions)).

insert_adjacent(Command,Target, Elements) ->
    Pid = self(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,wf_context:actions()} end),
    {Render,Actions} = receive A -> A end,
    wf:wire(wf:f("qi('~s').insertAdjacentHTML('~s', '~s');",[Target,Command,Render])),
    wf:wire(wf:render(Actions)).

insert_top(Target, #tr{} = Elements)    -> insert_top(tbody,Target, Elements);
insert_top(Target, Elements)            -> insert_top('div',Target, Elements).
insert_bottom(Target, #tr{} = Elements) -> insert_bottom(tbody, Target, Elements);
insert_bottom(Target, Elements)         -> insert_bottom('div', Target, Elements).
insert_before(Target, Elements)         -> insert_adjacent(beforebegin,Target, Elements).
insert_after(Target, Elements)          -> insert_adjacent(afterend,Target, Elements).

remove(Target) ->
    wf:wire(wf:f("qi('~s').parentNode.removeChild(qi('~s'));",[Target,Target])).

% Wire JavaScript wf:wire

wire(Actions) -> action_wire:wire(Actions).

% Spawn async processes wf:async, wf:flush

comet(Function) -> async(Function). % legacy name
async(Function) -> action_async:async(Function).
async(Name,Function) -> action_async:async(Name,Function).
flush(Key) -> action_async:flush(Key).

% Redirect and purge connection wf:redirect

redirect(Url) ->
    wf:wire(#jq{target='window.top',property=location,args=simple,right=Url}).

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

% Process State

state(Key) -> erlang:get(Key).
state(Key,Value) -> erlang:put(Key,Value).

% Context Variables and URL Query Strings wf:q and wf:qs

q(Key) -> Val = get(Key), case Val of undefined -> qs(Key); A -> A end.
qp(Key,Ctx) -> proplists:get_value(Key,Ctx#cx.params).
qs(Key) -> proplists:get_value(Key,?CTX#cx.params).
dqs(Key) -> proplists:get_value(Key,?CTX#cx.form).

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
form(Req) -> ?BRIDGE:form(Req).
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

-define(LOGGER, (wf:config(n2o,log_backend,n2o_io))).
log_modules() -> [wf].
-define(ALLOWED, (wf:config(n2o,log_modules,wf))).

log(Module, String, Args, Fun) ->
    case lists:member(Module, ?ALLOWED:log_modules()) of
        true -> ?LOGGER:Fun(Module, String, Args);
        false -> skip end.

info(Module, String, Args) -> log(Module, String, Args, info).
info(String, Args) -> log(?MODULE, String, Args, info).
info(String) -> log(?MODULE, String, [], info).

warning(Module, String, Args) -> log(Module, String, Args, warning).
warning(String, Args) -> log(?MODULE, String, Args, warning).
warning(String) -> log(?MODULE, String, [], warning).

error(Module, String, Args) -> log(Module, String, Args, error).
error(String, Args) -> log(?MODULE, String, Args, error).
error(String) -> log(?MODULE, String, [], error).

% Convert and Utils API

display(Element,Status) -> 
   wf:wire("{ var x = document.getElementById('"++
      wf:to_list(Element)++"'); if (x) x.style.display = '"++wf:to_list(Status)++"'; }").

show(Element) -> display(Element,block).
hide(Element) -> display(Element,none).

atom(List) when is_list(List) -> wf:to_atom(string:join([ wf:to_list(L) || L <- List],"_"));
atom(Scalar) -> wf:to_atom(Scalar).

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

temp_id() -> {_, _, C} = now(), "auto" ++ integer_to_list(C).
append(List, Key, Value) -> case Value of undefined -> List; _A -> [{Key, Value}|List] end.
render(X) -> wf_render:render(X).

actions() -> wf_context:actions().
actions(Ac) -> wf_context:actions(Ac).
context() -> wf_context:context().
context(Cx) -> wf_context:context(Cx).
context(Cx,Proto) -> wf_context:context(Cx,Proto).
context(Cx,Proto,UserCx) -> wf_context:context(Cx,Proto,UserCx).
script() -> wf_context:script().
script(Script) -> wf_context:script(Script).
add_action(Action) -> wf_context:add_action(Action).

config_multiple(Keys) -> [config(Key, "") || Key <- Keys].
config(Key) -> config(n2o, Key, "").
config(App, Key) -> config(App,Key, "").
config(App, Key, Default) -> wf_utils:config(App, Key, Default).

version() -> "1.10.0".

setkey(Name,Pos,List,New) ->
    case lists:keyfind(Name,Pos,List) of
        false -> [New|List];
        Element -> lists:keyreplace(Name,Pos,List,New) end.
