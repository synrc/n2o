-module(wf).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile (export_all).

% Here is Nitrogen Web Framework compatible API
% Please read major changes made to N2O and
% how to port existing Nitrogen sites at http://synrc.com/framework/web/

% N2O API compatible with Nitrogen
% ================================
% http://synrc.com/framework/web/api.htm

% Update DOM wf:update

update(Target, Elements) -> wf:wire(#jq{target=Target,method=[html],args=[wf:f("'~s'",[wf_core:render(Elements)])]}).
replace(Target, Elements) -> wf:wire(#jq{target=Target,method=[replaceWith],args=[wf:f("'~s'",[wf_core:render(Elements)])]}).
insert_top(Target, Elements) -> wf:wire(#jq{target=Target,method=[prepend],args=[wf:f("'~s'",[wf_core:render(Elements)])]}).
insert_bottom(Target, Elements) -> wf:wire(#jq{target=Target,method=[append],args=[wf:f("'~s'",[wf_core:render(Elements)])]}).
insert_before(Target, Elements) -> wf:wire(#jq{target=Target,method=[before],args=[wf:f("'~s'",[wf_core:render(Elements)])]}).
insert_after(Target, Elements) -> wf:wire(#jq{target=Target,method=['after'],args=[wf:f("'~s'",[wf_core:render(Elements)])]}).
remove(Target) -> wf:wire(#jq{target=Target,method=[remove],args=[]}).

% Wire JavaScript wf:wire

wire(Actions) -> wire(undefined, undefined, Actions).
wire(Target, Actions) -> wire(Target, Target, Actions).
wire(Trigger, Target, Actions) -> action_wire:wire(Trigger, Target, Actions).

% Spawn async processes wf:async, wf:flush

comet(Function) -> async(Function). % legacy name
async(Function) -> action_async:async(Function).
async(Name,Function) -> action_async:async(Name,Function).
flush(Key) -> action_async:flush(Key).

% Redirect and purge connection wf:redirect

redirect(Url) -> wf:wire(#jq{target=window,property=location,right=["\"",Url,"\""]}).

% Message Bus communications wf:reg wf:send

-ifndef(REGISTRATOR).
-define(REGISTRATOR, n2o_gproc).
-endif.

send(Pool, Message) -> ?REGISTRATOR:send(Pool,Message).
reg(Pool) -> ?REGISTRATOR:reg(Pool).

% Pickling wf:pickle

-ifndef(PICKLER).
-define(PICKLER, (wf:config(n2o,pickler,wf_secret))).
-endif.

pickle(Data) -> ?PICKLER:pickle(Data).
depickle(SerializedData) -> ?PICKLER:depickle(SerializedData).
depickle(SerializedData, TTLSeconds) -> ?PICKLER:depickle(SerializedData, TTLSeconds).

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
logout() -> clear_user(), clear_roles(), clear_session().

% Context Variables and URL Query Strings wf:q and wf:qs

q(Key) -> Val = get(Key), case Val of undefined -> qs(Key); A -> A end.
qs(Key) -> proplists:get_value(Key,?CTX#context.params).

% Bridge Information

-ifndef(BRIDGE).
-define(BRIDGE, n2o_cowboy).
-endif.

cookie(Cookie,Req) -> ?BRIDGE:cookie(Cookie, Req).
params(Req) -> ?BRIDGE:params(Req).
cookies(Req) -> ?BRIDGE:cookies(Req).
headers(Req) -> ?BRIDGE:headers(Req).
peer(Req) -> ?BRIDGE:peer(Req).
path(Req) -> ?BRIDGE:path(Req).
request_body(Req) -> ?BRIDGE:request_body(Req).
delete_cookie(Cookie,Req) -> ?BRIDGE:delete_cookie(Cookie,Req).
header(Name,Val,Req) -> ?BRIDGE:header(Name, Val, Req).
response(Html,Req) -> ?BRIDGE:response(Html,Req).
reply(Status,Req) -> ?BRIDGE:reply(Status,Req).

% Logging API

info(String, Args) ->  error_logger:info_msg(String, Args).
info(String) -> error_logger:info_msg(String).
warning(String, Args) -> error_logger:warning_msg(String, Args).
warning(String) -> error_logger:warning_msg(String).
error(String, Args) -> error_logger:error_msg(String, Args).
error(String) -> error_logger:error_msg(String).

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
to_js_id(Path) -> _String = wf_render_actions:to_js_id(Path).

% These api are not really API

temp_id() -> {_, _, C} = now(), "temp" ++ integer_to_list(C).
append(List, Key, Value) -> case Value of undefined -> List; _A -> [{Key, Value}|List] end.
render(X) -> wf_core:render(X).

config_multiple(Keys) -> [config(Key, "") || Key <- Keys].
config(Key) -> config(n2o, Key, "").
config(App,Key) -> config(App,Key, "").
config(App, Key, Default) -> case application:get_env(App,Key) of
                              undefined -> Default;
                              {ok,V} -> V end.
