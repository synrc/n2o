-module(wf).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

% Here is Nitrogen Web Framework compatible API
% Please read major changes made to N2O and how
% to port existing nitrogen sites at http://synrc.com/framework/web/

% N2O Core API compatible with Nitrogen
% =====================================

% Update DOM wf:update

set(Element, Value) -> action_set:set(Element, Value).
update(Target, Elements) -> action_update:update(Target, Elements).
replace(Target, Elements) -> action_update:replace(Target, Elements).
insert_top(Target, Elements) -> action_update:insert_top(Target, Elements).
insert_bottom(Target, Elements) -> action_update:insert_bottom(Target, Elements).
insert_before(Target, Elements) -> action_update:insert_before(Target, Elements).
insert_after(Target, Elements) -> action_update:insert_after(Target, Elements).
remove(Target) -> action_update:remove(Target).

% Wire JavaScript wf:wire

wire(Actions) -> wire(undefined, undefined, Actions).
wire(Target, Actions) -> wire(Target, Target, Actions).
wire(Trigger, Target, Actions) -> action_wire:wire(Trigger, Target, Actions).

% Spawn async processes wf:comet wf:flush

comet(Function) -> action_comet:comet(Function).
flush(Key) -> action_comet:flush(Key).

% Parse URL and context parameters wf:q

%q(Key) -> get(Key).
%mq(KeyList) when is_list(KeyList) -> [q(X) || X<-KeyList].
%mqs(KeyList) when is_list(KeyList) -> [qs(X) || X<-KeyList].
%q_pl(KeyList) when is_list(KeyList) -> [{K,q(K)} || K <- KeyList].
%qs_pl(KeyList) when is_list(KeyList) -> [{K,qs(K)} || K <- KeyList].
%params() -> query_handler:get_params().

% Redirect and purge connection wf:redirect

redirect(Url) -> action_redirect:redirect(Url).
redirect_to_login(LoginUrl) -> action_redirect:redirect_to_login(LoginUrl).
redirect_from_login(DefaultUrl) -> action_redirect:redirect_from_login(DefaultUrl).

% GProc process registration wf:reg wf:send

-ifndef(REGISTRATOR).
-define(REGISTRATOR, n2o_gproc).
-endif.

send(Pool, Message) -> ?REGISTRATOR:send(Pool,Message).
reg(Pool) -> ?REGISTRATOR:reg(Pool).

% Pickling wf:pickle

pickle(Data) -> wf_pickle:pickle(Data).
depickle(SerializedData) -> wf_pickle:depickle(SerializedData).
depickle(SerializedData, TTLSeconds) -> wf_pickle:depickle(SerializedData, TTLSeconds).

% Session handling wf:session wf:user wf:role

session(Key) -> session_handler:get_value(Key).
session(Key, Value) -> session_handler:set_value(Key, Value).
session_default(Key, DefaultValue) -> session_handler:get_value(Key, DefaultValue).
clear_session() -> session_handler:clear_all().
session_id() -> session_handler:session_id().
user() -> identity_handler:get_user().
user(User) -> identity_handler:set_user(User).
clear_user() -> identity_handler:clear().
role(Role) -> role_handler:get_has_role(Role).
role(Role, IsInRole) -> role_handler:set_has_role(Role, IsInRole).
roles() -> role_handler:get_roles().
clear_roles() -> role_handler:clear_all().

% Bridge Information

-ifndef(BRIDGE).
-define(BRIDGE, n2o_cowboy).
-endif.

q(Key) -> Val = get(Key), case Val of undefined -> qs(Key); A -> A end.
qs(Key) -> proplists:get_value(Key,wf_context:params()).
params(Req) -> ?BRIDGE:params(Req).
cookies(Req) -> ?BRIDGE:cookies(Req).
cookie(Cookie) -> ?BRIDGE:cookie(Cookie,(wf_context:context())#context.req).
cookie(Cookie,Req) -> ?BRIDGE:cookie(Cookie,Req).
cookie(Cookie, Value, Req) -> ?BRIDGE:cookie(Cookie, Value, Req).
cookie(Cookie, Value, Path, MinutesToLive, Req) -> ?BRIDGE:cookie(Cookie, Value, Path, MinutesToLive, Req).
headers(Req) -> ?BRIDGE:headers(Req).
peer(Req) -> ?BRIDGE:peer(Req).
path(Req) -> ?BRIDGE:path(Req).
request_body(Req) -> ?BRIDGE:request_body(Req).
delete_cookie(Cookie,Req) -> ?BRIDGE:delete_cookie(Cookie,Req).
header(Name, Val, Req) -> ?BRIDGE:header(Name, Val, Req).
response(Html,Req) -> ?BRIDGE:response(Html,Req).
reply(Status,Req) -> ?BRIDGE:reply(Status,Req).

% Logging API

info(String, Args) ->  error_logger:info_msg(String, Args).
info(String) -> error_logger:info_msg(String).
warning(String, Args) -> error_logger:warning_msg(String, Args).
warning(String) -> error_logger:warning_msg(String).
error(String, Args) -> error_logger:error_msg(String, Args).
error(String) -> error_logger:error_msg(String).

% Compatibility Obsolete API
% ==========================


% Q: Do we need converting API ?

comet_global(Function, _Pool) -> action_comet:comet(Function).
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
to_js_id(Path) -> _String = wf_render_actions:to_js_id(Path).

% Q: Why we need state if already has session process dictionary ?

state(Key) -> get(Key).
state_default(Key, DefaultValue) -> case get(Key) of undefined -> DefaultValue; A -> A end.
state(Key, Value) -> put(Key,Value).
clear_state(Key) -> put(Key,undefined).

% Q: Do we really need continuations ? Who using it ?

continue(Tag, Function) -> action_continue:continue(Tag, Function).
continue(Tag, Function, TimeoutMS) -> action_continue:continue(Tag, Function, TimeoutMS).

% These api are not really API

temp_id() -> _String = wf_render_elements:temp_id().
normalize_id(Path) -> _String = wf_render_elements:normalize_id(Path).
send_global(Pool, Message) -> ok = action_comet:send_global(Pool, Message).
comet(Function, _Pool) ->  action_comet:comet(Function).
logout() -> clear_user(), clear_roles(), clear_session().
flash(Elements) -> element_flash:add_flash(Elements).
flash(FlashID, Elements) -> element_flash:add_flash(FlashID, Elements).
async_mode() -> wf_context:async_mode().
async_mode(AsyncMode) -> wf_context:async_mode(AsyncMode).
switch_to_comet() -> async_mode(comet).
switch_to_polling(IntervalInMS) -> async_mode({poll, IntervalInMS}).
debug() -> wf_utils:debug().
break() -> wf_utils:break().
assert(true, _) -> ok;
assert(false, Error) -> erlang:error(Error).

append(List, Key, Value) -> case Value of undefined -> List; _A -> [{Key, Value}|List] end.
render(X) -> wf_core:render(X).

config_multiple(Keys) -> [config(Key, "") || Key <- Keys].
config(Key) -> config(n2o, Key, "").
config(App,Key) -> config(App,Key, "").
config(App, Key, Default) -> case application:get_env(App,Key) of
                              undefined -> Default;
                              {ok,V} -> V end.
