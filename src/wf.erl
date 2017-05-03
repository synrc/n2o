-module(wf).
-author('Rusty Klophaus').
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-include_lib("n2o/include/api.hrl").
-compile (export_all).

-define(ACTION_BASE(Module), ancestor=action, trigger, target, module=Module, actions, source=[]).
-record(jq,      {?ACTION_BASE(action_jq), property, method, args=[], right, format="~s"}).

% Here is Nitrogen Web Framework compatible API
% Please read major changes made to N2O and
% how to port existing Nitrogen sites at http://synrc.com/apps/n2o

% Update DOM wf:update

update(Target, Elements) ->
    wf:wire(#jq{target=Target,property=outerHTML,right=Elements,format="'~s'"}).

insert_top(Tag,Target, Elements) ->
    Pid = self(),
    Ref = make_ref(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,Ref,wf:actions()} end),
    {Render,Ref,Actions} = receive {_, Ref, _} = A -> A end,
    wf:wire(wf:f(
        "qi('~s').insertBefore("
        "(function(){var div = qn('~s'); div.innerHTML = '~s'; return div.firstChild; })(),"
        "qi('~s').firstChild);",
        [Target,Tag,Render,Target])),
    wf:wire(wf:render(Actions)).

insert_bottom(Tag, Target, Elements) ->
    Pid = self(),
    Ref = make_ref(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,Ref,wf:actions()} end),
    {Render,Ref,Actions} = receive {_, Ref, _} = A -> A end,
    wf:wire(wf:f(
        "(function(){ var div = qn('~s'); div.innerHTML = '~s';"
                     "qi('~s').appendChild(div.firstChild); })();",
        [Tag,Render,Target])),
    wf:wire(wf:render(Actions)).

insert_adjacent(Command,Target, Elements) ->
    Pid = self(),
    Ref = make_ref(),
    spawn(fun() -> R = wf:render(Elements), Pid ! {R,Ref,wf:actions()} end),
    {Render,Ref,Actions} = receive {_, Ref, _} = A -> A end,
    wf:wire(wf:f("qi('~s').insertAdjacentHTML('~s', '~s');",[Target,Command,Render])),
    wf:wire(wf:render(Actions)).

insert_top(Target, Elements) when element(1,Elements) == tr -> insert_top(tbody,Target, Elements);
insert_top(Target, Elements) -> insert_top('div',Target, Elements).
insert_bottom(Target, Elements) when element(1,Elements) == tr -> insert_bottom(tbody, Target, Elements);
insert_bottom(Target, Elements) -> insert_bottom('div', Target, Elements).
insert_before(Target, Elements) -> insert_adjacent(beforebegin,Target, Elements).
insert_after(Target, Elements) -> insert_adjacent(afterend,Target, Elements).

remove(Target) ->
    wf:wire("var x=qi('"++wf:to_list(Target)++"'); x && x.parentNode.removeChild(x);").

% Wire JavaScript wf:wire

wire(Actions) -> action_wire:wire(Actions).

% Spawn async processes wf:async, wf:flush

async(Function) -> n2o_async:async(Function).
start(#handler{}=Handler) -> n2o_async:start(Handler).
stop(Name) -> n2o_async:stop(Name).
restart(Name) -> n2o_async:restart(Name).
async(Name,Function) -> n2o_async:async(Name,Function).
flush() -> n2o_async:flush().
flush(Key) -> n2o_async:flush(Key).

% Redirect and purge connection wf:redirect

redirect({http,Url}) -> wf:header(<<"Location">>,wf:to_binary(Url)), wf:state(status,302), [];
redirect(Url) -> wf:wire(#jq{target='window.top',property=location,args=simple,right=Url}).
header(K,V) -> wf:context((?CTX)#cx{req=wf:header(K,V,?REQ)}).

% Message Bus communications wf:reg wf:send

-ifndef(REGISTRATOR).
-define(REGISTRATOR, (wf:config(n2o,mq,n2o_mq))).
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
invalidate_cache() -> ets:foldl(fun(X,A) -> wf:cache(element(1,X)) end, 0, caching).
cache(Key, undefined) -> ets:delete(caching,Key);
cache(Key, Value) -> ets:insert(caching,{Key,n2o_session:till(calendar:local_time(), n2o_session:ttl()),Value}), Value.
cache(Key, Value, Till) -> ets:insert(caching,{Key,Till,Value}), Value.
cache(Key) ->
    Res = ets:lookup(caching,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined;
                {_,infinity,X} -> X;
                {_,Expire,X} -> case Expire < calendar:local_time() of
                                  true ->  ets:delete(caching,Key), undefined;
                                  false -> X end end.

% Process State

state(Key) -> erlang:get(Key).
state(Key,Value) -> erlang:put(Key,Value).

% Context Variables and URL Query Strings from ?REQ and ?CTX wf:q wf:qc wf:qp

q(Key) -> Val = get(Key), case Val of undefined -> qc(Key); A -> A end.
qc(Key) -> qc(Key,?CTX).
qc(Key,Ctx) -> proplists:get_value(to_binary(Key),Ctx#cx.params).
qp(Key) -> qp(Key,?REQ).
qp(Key,Req) -> {Params,_} = params(Req), proplists:get_value(to_binary(Key),Params).
lang() -> ?CTX#cx.lang.

% Cookies

cookies() -> n2o_cx:cookies().
cookie(Name) -> lists:keyfind(Name,1,cookies()).
cookie(Name,Value) -> cookie(Name,Value,"/", 24 * 60 * 60).
cookie(Name,Value,Path,TTL) -> n2o_cx:add_cookie(Name,Value,Path,TTL).

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

-define(LOGGER, (wf:config(n2o,log_backend,n2o_log))).
log_modules() -> [wf].
log_level() -> info.
-define(LOG_MODULES, (wf:config(n2o,log_modules,wf))).
-define(LOG_LEVEL, (wf:config(n2o,log_level,wf))).

log_level(none) -> 3;
log_level(error) -> 2;
log_level(warning) -> 1;
log_level(_) -> 0.

log(Module, String, Args, Fun) ->
    case log_level(Fun) < log_level(?LOG_LEVEL:log_level()) of
        true -> skip;
        false -> case ?LOG_MODULES:log_modules() of
            any -> ?LOGGER:Fun(Module, String, Args);
            Allowed -> case lists:member(Module, Allowed) of
                true -> ?LOGGER:Fun(Module, String, Args);
                false -> skip end end end.

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
   wf:wire("{ var x = qi('"++
      wf:to_list(Element)++"'); if (x) x.style.display = '"++wf:to_list(Status)++"'; }").

show(Element) -> display(Element,block).
hide(Element) -> display(Element,none).

atom(List) when is_list(List) -> wf:to_atom(string:join([ wf:to_list(L) || L <- List],"_"));
atom(Scalar) -> wf:to_atom(Scalar).

f(S)        -> wf_utils:f(S).
f(S, Args)  -> wf_utils:f(S, Args).
coalesce(L) -> wf_utils:coalesce(L).
json(Json)  -> jsone:encode(Json).

to_list(T)    -> wf_convert:to_list(T).
to_atom(T)    -> wf_convert:to_atom(T).
to_binary(T)  -> wf_convert:to_binary(T).
to_integer(T) -> wf_convert:to_integer(T).

jse(String)            -> wf_convert:js_escape(String).
js_escape(String)      -> wf_convert:js_escape(String).
hte(S)                 -> wf_convert:html_encode(S).
hte(S, Encode)         -> wf_convert:html_encode(S, Encode).
html_encode(S)         -> wf_convert:html_encode(S).
html_encode(S, Encode) -> wf_convert:html_encode(S, Encode).
url_encode(S)          -> wf_convert:url_encode(S).
url_decode(S)          -> wf_convert:url_decode(S).
hex_encode(S)          -> wf_convert:hex(S).
hex_decode(S)          -> wf_convert:unhex(S).
join(List,Delimiter)   -> wf_convert:join(List,Delimiter).
format(Term)           -> wf_convert:format(Term).

% These api are not really API

unique_integer() -> try erlang:unique_integer() catch _:_ -> {MS,S,US} = erlang:now(), (MS*1000000+S)*1000000+US end.
temp_id() -> "auto" ++ integer_to_list(unique_integer() rem 1000000).
append(List, Key, Value) -> case Value of undefined -> List; _A -> [{Key, Value}|List] end.
render(X) -> wf_render:render(X).

init_context(R)-> n2o_cx:init_context(R).
actions()      -> n2o_cx:actions().
actions(Ac)    -> n2o_cx:actions(Ac).
script()       -> n2o_cx:script().
script(Script) -> n2o_cx:script(Script).
context()      -> n2o_cx:context().
context(Cx)    -> n2o_cx:context(Cx).
context(Cx,Proto)        -> n2o_cx:context(Cx,Proto).
context(Cx,Proto,UserCx) -> n2o_cx:context(Cx,Proto,UserCx).
add_action(Action)       -> n2o_cx:add_action(Action).
fold(Fun,Handlers,Ctx) -> n2o_cx:fold(Fun,Handlers,Ctx).

config_multiple(Keys) -> [config(Key, "") || Key <- Keys].
config(Key) -> config(n2o, Key, "").
config(App, Key) -> config(App,Key, "").
config(App, Key, Default) -> wf_utils:config(App, Key, Default).

version() -> proplists:get_value(vsn,element(2,application:get_all_key(n2o))).

setkey(Name,Pos,List,New) ->
    case lists:keyfind(Name,Pos,List) of
        false -> [New|List];
        _Element -> lists:keyreplace(Name,Pos,List,New) end.
