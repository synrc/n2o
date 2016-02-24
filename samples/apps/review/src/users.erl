-module(users).
-behaviour(rest).
-compile({parse_transform, rest}).
-include_lib("kvs/include/user.hrl").
-export([init/0, populate/1, exists/1, get/0, get/1, post/1, delete/1]).
-rest_record(user).

init()               -> ets:new(users, [public, named_table, {keypos, #user.id}]).
populate(Users)      -> ets:insert(users, Users).
exists(Id)           -> ets:member(users, wf:to_list(Id)).
get()                -> ets:tab2list(users).
get(Id)              -> #user{id=Id}.
delete(Id)           -> ets:delete(users, wf:to_list(Id)).
post(#user{} = User) -> ets:insert(users, User), true;
post(Data)           -> post(from_json(Data, #user{})), true.
